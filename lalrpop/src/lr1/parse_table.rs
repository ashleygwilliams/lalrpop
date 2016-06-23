//! A compiler from an LR(1) table to a traditional table driven parser.

use collections::{Multimap, Set};
use grammar::repr::{Grammar,
                    NonterminalString,
                    Production,
                    Symbol,
                    TerminalString, TypeParameter, TypeRepr, Types};
use lr1::core::*;
use lr1::lookahead::Token;
use lr1::state_graph::StateGraph;
use lr1::tls::Lr1Tls;
use rust::RustWrite;
use std::io::{self, Write};
use tls::Tls;
use util::{Escape, Sep};

pub fn compile<'grammar,W:Write>(
    grammar: &'grammar Grammar,
    user_start_symbol: NonterminalString,
    start_symbol: NonterminalString,
    states: &[LR1State<'grammar>],
    out: &mut RustWrite<W>)
    -> io::Result<()>
{
    let _lr1_tls = Lr1Tls::install(grammar.terminals.clone());
    let graph = StateGraph::new(&states);
    let mut ascent = TableDriven::new(grammar, user_start_symbol, start_symbol,
                                      &graph, states, out);
    ascent.write()
}

// For each state, we will create a table. The table is indexed by the
// index of the next token. The value in the table is an `i32`, and
// its interpretation varies depending on whether it is positive
// or negative:
//
// - if zero or a positive integer, it is the next state to shift to.
// - if a negative integer, it is the index of a reduction
//   action to execute (actually index + 1).
//
// We maintain two stacks: one is a stack of state indexes (each an
// u32). The other is a stack of values and spans: `(L, T, L)`. `L` is
// the location type and represents the start/end span. `T` is the
// value of the symbol. The type `T` is an `enum` that we synthesize
// which contains a variant for all the possibilities:
//
// ```
// enum Value<> {
//     // One variant for each terminal:
//     Term1(Ty1),
//     ...
//     TermN(TyN),
//
//     // One variant for each nonterminal:
//     Nt1(Ty1),
//     ...
//     NtN(TyN),
// }
// ```

struct TableDriven<'ascent,'grammar:'ascent,W:Write+'ascent> {
    /// the complete grammar
    grammar: &'grammar Grammar,

    /// some suitable prefix to separate our identifiers from the user's
    prefix: &'grammar str,

    /// types from the grammar
    types: &'grammar Types,

    /// the start symbol S the user specified
    user_start_symbol: NonterminalString,

    /// the synthetic start symbol S' that we specified
    start_symbol: NonterminalString,

    /// the vector of states
    states: &'ascent [LR1State<'grammar>],

    /// where we write output
    out: &'ascent mut RustWrite<W>,

    /// type parameters for the `Nonterminal` type
    symbol_type_params: Vec<TypeParameter>,
}

impl<'ascent,'grammar,W:Write> TableDriven<'ascent,'grammar,W> {
    fn new(grammar: &'grammar Grammar,
           user_start_symbol: NonterminalString,
           start_symbol: NonterminalString,
           states: &'ascent [LR1State<'grammar>],
           out: &'ascent mut RustWrite<W>)
           -> RecursiveAscent<'ascent,'grammar,W>
    {
        // The nonterminal type needs to be parameterized by all the
        // type parameters that actually appear in the types of
        // nonterminals.  We can't just use *all* type parameters
        // because that would leave unused lifetime/type parameters in
        // some cases.
        let referenced_ty_params: Set<TypeParameter> =
            grammar.types.nonterminal_types()
                         .into_iter()
                         .chain(grammar.types.terminal_types())
                         .flat_map(|t| t.referenced())
                         .collect();

        let symbol_type_params: Vec<_> =
            grammar.type_parameters.iter()
                                   .filter(|t| referenced_ty_params.contains(t))
                                   .cloned()
                                   .collect();

        TableDriven {
            grammar: grammar,
            prefix: &grammar.prefix,
            types: &grammar.types,
            states: states,
            user_start_symbol: user_start_symbol,
            start_symbol: start_symbol,
            out: out,
            symbol_type_params: symbol_type_params,
        }
    }

    fn write(&mut self) -> io::Result<()> {
        rust!(self.out, "");
        rust!(self.out, "mod {}parse{} {{",
              self.prefix, self.start_symbol);

        // these stylistic lints are annoying for the generated code,
        // which doesn't follow conventions:
        rust!(self.out, "#![allow(non_snake_case, non_camel_case_types, \
                         unused_mut, unused_variables, unused_imports)]");
        rust!(self.out, "");

        try!(self.write_uses());

        

        rust!(self.out, "}}");
        Ok(())
    }

    fn write_uses(&mut self) -> io::Result<()> {
        try!(self.out.write_uses("super::", &self.grammar));

        if self.grammar.intern_token.is_none() {
            rust!(self.out, "use super::{}ToTriple;", self.prefix);
        }

        Ok(())
    }

    fn write_value_type_defn(&mut self) -> io::Result<()> {
        // sometimes some of the variants are not used, particularly
        // if we are generating multiple parsers from the same file:
        rust!(self.out, "#[allow(dead_code)]");
        rust!(self.out, "pub enum {}Symbols<{}> {{",
              self.prefix,
              Sep(", ", &self.symbol_type_params));

        // make an enum with one variant per terminal
        for &term in &self.grammar.terminals.all {
            let ty = self.types.terminal_type(term).clone();
            rust!(self.out, "{}({}),", Escape(term), ty);
        }

        // make an enum with one variant per nonterminal
        for &nt in self.grammar.nonterminals.keys() {
            let ty = self.types.nonterminal_type(nt).clone();
            rust!(self.out, "{}({}),", Escape(nt), ty);
        }

        rust!(self.out, "}}");
        Ok(())
    }

}
