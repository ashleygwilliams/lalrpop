#![allow(unused_imports)]
#![allow(unused_variables)]
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};
use std::str::FromStr;
use util::tok::Tok;
extern crate lalrpop_util as __lalrpop_util;
use self::__lalrpop_util::ParseError as __ParseError;

mod __parse__Expr {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use std::fmt::Debug;
    use std::ops::{Add, Div, Mul, Sub};
    use std::str::FromStr;
    use util::tok::Tok;
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    pub fn parse_Expr<
        'input,
        F,
    >(
        input: &'input str,
    ) -> Result<F, __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __tokens = super::__intern_token::__Matcher::new(input);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match try!(__state0(input, &mut __tokens, __lookahead)) {
            (Some(__lookahead), _) => {
                Err(__ParseError::ExtraToken { token: __lookahead })
            }
            (None, __Nonterminal::____Expr((_, __nt, _))) => {
                Ok(__nt)
            }
            _ => unreachable!(),
        }
    }

    #[allow(dead_code)]
    pub enum __Nonterminal<F> {
        Expr((usize, F, usize)),
        Factor((usize, F, usize)),
        Term((usize, F, usize)),
        ____Expr((usize, F, usize)),
    }

    // State 0
    //     AllInputs = []
    //     OptionalInputs = []
    //     FixedInputs = []
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = (*) Expr "+" Factor ["+"]
    //     Expr = (*) Expr "+" Factor ["-"]
    //     Expr = (*) Expr "+" Factor [EOF]
    //     Expr = (*) Expr "-" Factor ["+"]
    //     Expr = (*) Expr "-" Factor ["-"]
    //     Expr = (*) Expr "-" Factor [EOF]
    //     Expr = (*) Factor ["+"]
    //     Expr = (*) Factor ["-"]
    //     Expr = (*) Factor [EOF]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["+"]
    //     Factor = (*) Factor "*" Term ["-"]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "*" Term [EOF]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["+"]
    //     Factor = (*) Factor "/" Term ["-"]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Factor "/" Term [EOF]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["+"]
    //     Factor = (*) Term ["-"]
    //     Factor = (*) Term ["/"]
    //     Factor = (*) Term [EOF]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["+"]
    //     Term = (*) "(" Expr ")" ["-"]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = (*) "(" Expr ")" [EOF]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["+"]
    //     Term = (*) r#"[0-9]+"# ["-"]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //     Term = (*) r#"[0-9]+"# [EOF]
    //     __Expr = (*) Expr [EOF]
    //
    //   "(" -> S4
    //   r#"[0-9]+"# -> S5
    //
    //     Expr -> S1
    //     Factor -> S2
    //     Term -> S3
    pub fn __state0<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym0 = (__loc1, (__tok0), __loc2);
                __result = try!(__state4(input, __tokens, __sym0));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym0 = (__loc1, (__tok0), __loc2);
                __result = try!(__state5(input, __tokens, __sym0));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Expr(__sym0) => {
                    __result = try!(__state1(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::Factor(__sym0) => {
                    __result = try!(__state2(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::Term(__sym0) => {
                    __result = try!(__state3(input, __tokens, __lookahead, __sym0));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 1
    //     AllInputs = [Expr]
    //     OptionalInputs = []
    //     FixedInputs = [Expr]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Expr (*) "+" Factor ["+", "-", EOF]
    //     Expr = Expr (*) "-" Factor ["+", "-", EOF]
    //     __Expr = Expr (*) [EOF]
    //
    //   "+" -> S6
    //   "-" -> S7
    //   [EOF] -> __Expr = Expr => ActionFn(0);
    //
    pub fn __state1<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (3, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state6(input, __tokens, __sym0, __sym1));
                return Ok(__result);
            }
            Some((__loc1, (4, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state7(input, __tokens, __sym0, __sym1));
                return Ok(__result);
            }
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0(input, __sym0);
                let __nt = __Nonterminal::____Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 2
    //     AllInputs = [Factor]
    //     OptionalInputs = []
    //     FixedInputs = [Factor]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Factor (*) ["+", "-", EOF]
    //     Factor = Factor (*) "*" Term ["*", "+", "-", "/", EOF]
    //     Factor = Factor (*) "/" Term ["*", "+", "-", "/", EOF]
    //
    //   "*" -> S8
    //   "/" -> S9
    //   ["+", "-", EOF] -> Expr = Factor => ActionFn(3);
    //
    pub fn __state2<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state8(input, __tokens, __sym0, __sym1));
                return Ok(__result);
            }
            Some((__loc1, (5, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state9(input, __tokens, __sym0, __sym1));
                return Ok(__result);
            }
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 3
    //     AllInputs = [Term]
    //     OptionalInputs = []
    //     FixedInputs = [Term]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Factor)
    //
    //     Factor = Term (*) ["*", "+", "-", "/", EOF]
    //
    //   ["*", "+", "-", "/", EOF] -> Factor = Term => ActionFn(6);
    //
    pub fn __state3<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) |
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action6(input, __sym0);
                let __nt = __Nonterminal::Factor((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 4
    //     AllInputs = ["("]
    //     OptionalInputs = []
    //     FixedInputs = ["("]
    //     WillPushLen = 2
    //     WillPush = [Expr, ")"]
    //     WillProduce = Some(Term)
    //
    //     Expr = (*) Expr "+" Factor [")"]
    //     Expr = (*) Expr "+" Factor ["+"]
    //     Expr = (*) Expr "+" Factor ["-"]
    //     Expr = (*) Expr "-" Factor [")"]
    //     Expr = (*) Expr "-" Factor ["+"]
    //     Expr = (*) Expr "-" Factor ["-"]
    //     Expr = (*) Factor [")"]
    //     Expr = (*) Factor ["+"]
    //     Expr = (*) Factor ["-"]
    //     Factor = (*) Factor "*" Term [")"]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["+"]
    //     Factor = (*) Factor "*" Term ["-"]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "/" Term [")"]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["+"]
    //     Factor = (*) Factor "/" Term ["-"]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Term [")"]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["+"]
    //     Factor = (*) Term ["-"]
    //     Factor = (*) Term ["/"]
    //     Term = (*) "(" Expr ")" [")"]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["+"]
    //     Term = (*) "(" Expr ")" ["-"]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = "(" (*) Expr ")" ["*", "+", "-", "/", EOF]
    //     Term = (*) r#"[0-9]+"# [")"]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["+"]
    //     Term = (*) r#"[0-9]+"# ["-"]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //
    //   "(" -> S13
    //   r#"[0-9]+"# -> S14
    //
    //     Expr -> S10
    //     Factor -> S11
    //     Term -> S12
    pub fn __state4<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        let __sym0 = &mut Some(__sym0);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state13(input, __tokens, __sym1));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state14(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            if __sym0.is_none() {
                return Ok(__result);
            }
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Expr(__sym1) => {
                    __result = try!(__state10(input, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::Factor(__sym1) => {
                    __result = try!(__state11(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::Term(__sym1) => {
                    __result = try!(__state12(input, __tokens, __lookahead, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 5
    //     AllInputs = [r#"[0-9]+"#]
    //     OptionalInputs = []
    //     FixedInputs = [r#"[0-9]+"#]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Term)
    //
    //     Term = r#"[0-9]+"# (*) ["*", "+", "-", "/", EOF]
    //
    //   ["*", "+", "-", "/", EOF] -> Term = r#"[0-9]+"# => ActionFn(7);
    //
    pub fn __state5<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) |
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7(input, __sym0);
                let __nt = __Nonterminal::Term((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 6
    //     AllInputs = [Expr, "+"]
    //     OptionalInputs = []
    //     FixedInputs = [Expr, "+"]
    //     WillPushLen = 1
    //     WillPush = [Factor]
    //     WillProduce = Some(Expr)
    //
    //     Expr = Expr "+" (*) Factor ["+", "-", EOF]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["+", "-", EOF]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["+", "-", EOF]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["+", "-", EOF]
    //     Factor = (*) Term ["/"]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["+", "-", EOF]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["+", "-", EOF]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //
    //   "(" -> S4
    //   r#"[0-9]+"# -> S5
    //
    //     Factor -> S15
    //     Term -> S3
    pub fn __state6<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        let __sym0 = &mut Some(__sym0);
        let __sym1 = &mut Some(__sym1);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state4(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state5(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            if __sym1.is_none() {
                return Ok(__result);
            }
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Factor(__sym2) => {
                    __result = try!(__state15(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state3(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 7
    //     AllInputs = [Expr, "-"]
    //     OptionalInputs = []
    //     FixedInputs = [Expr, "-"]
    //     WillPushLen = 1
    //     WillPush = [Factor]
    //     WillProduce = Some(Expr)
    //
    //     Expr = Expr "-" (*) Factor ["+", "-", EOF]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["+", "-", EOF]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["+", "-", EOF]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["+", "-", EOF]
    //     Factor = (*) Term ["/"]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["+", "-", EOF]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["+", "-", EOF]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //
    //   "(" -> S4
    //   r#"[0-9]+"# -> S5
    //
    //     Factor -> S16
    //     Term -> S3
    pub fn __state7<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        let __sym0 = &mut Some(__sym0);
        let __sym1 = &mut Some(__sym1);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state4(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state5(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            if __sym1.is_none() {
                return Ok(__result);
            }
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Factor(__sym2) => {
                    __result = try!(__state16(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state3(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 8
    //     AllInputs = [Factor, "*"]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "*"]
    //     WillPushLen = 1
    //     WillPush = [Term]
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "*" (*) Term ["*", "+", "-", "/", EOF]
    //     Term = (*) "(" Expr ")" ["*", "+", "-", "/", EOF]
    //     Term = (*) r#"[0-9]+"# ["*", "+", "-", "/", EOF]
    //
    //   "(" -> S4
    //   r#"[0-9]+"# -> S5
    //
    //     Term -> S17
    pub fn __state8<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state4(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state5(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state17(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                    return Ok(__result);
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 9
    //     AllInputs = [Factor, "/"]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "/"]
    //     WillPushLen = 1
    //     WillPush = [Term]
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "/" (*) Term ["*", "+", "-", "/", EOF]
    //     Term = (*) "(" Expr ")" ["*", "+", "-", "/", EOF]
    //     Term = (*) r#"[0-9]+"# ["*", "+", "-", "/", EOF]
    //
    //   "(" -> S4
    //   r#"[0-9]+"# -> S5
    //
    //     Term -> S18
    pub fn __state9<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state4(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state5(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state18(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                    return Ok(__result);
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 10
    //     AllInputs = ["(", Expr]
    //     OptionalInputs = ["("]
    //     FixedInputs = [Expr]
    //     WillPushLen = 1
    //     WillPush = [")"]
    //     WillProduce = None
    //
    //     Expr = Expr (*) "+" Factor [")", "+", "-"]
    //     Expr = Expr (*) "-" Factor [")", "+", "-"]
    //     Term = "(" Expr (*) ")" ["*", "+", "-", "/", EOF]
    //
    //   ")" -> S19
    //   "+" -> S20
    //   "-" -> S21
    //
    pub fn __state10<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                let __sym0 = __sym0.take().unwrap();
                __result = try!(__state19(input, __tokens, __sym0, __sym1, __sym2));
                return Ok(__result);
            }
            Some((__loc1, (3, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state20(input, __tokens, __sym1, __sym2));
                return Ok(__result);
            }
            Some((__loc1, (4, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state21(input, __tokens, __sym1, __sym2));
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 11
    //     AllInputs = [Factor]
    //     OptionalInputs = []
    //     FixedInputs = [Factor]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Factor (*) [")", "+", "-"]
    //     Factor = Factor (*) "*" Term [")", "*", "+", "-", "/"]
    //     Factor = Factor (*) "/" Term [")", "*", "+", "-", "/"]
    //
    //   "*" -> S22
    //   "/" -> S23
    //   [")", "+", "-"] -> Expr = Factor => ActionFn(3);
    //
    pub fn __state11<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state22(input, __tokens, __sym0, __sym1));
                return Ok(__result);
            }
            Some((__loc1, (5, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state23(input, __tokens, __sym0, __sym1));
                return Ok(__result);
            }
            Some((_, (1, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 12
    //     AllInputs = [Term]
    //     OptionalInputs = []
    //     FixedInputs = [Term]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Factor)
    //
    //     Factor = Term (*) [")", "*", "+", "-", "/"]
    //
    //   [")", "*", "+", "-", "/"] -> Factor = Term => ActionFn(6);
    //
    pub fn __state12<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action6(input, __sym0);
                let __nt = __Nonterminal::Factor((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 13
    //     AllInputs = ["("]
    //     OptionalInputs = []
    //     FixedInputs = ["("]
    //     WillPushLen = 2
    //     WillPush = [Expr, ")"]
    //     WillProduce = Some(Term)
    //
    //     Expr = (*) Expr "+" Factor [")"]
    //     Expr = (*) Expr "+" Factor ["+"]
    //     Expr = (*) Expr "+" Factor ["-"]
    //     Expr = (*) Expr "-" Factor [")"]
    //     Expr = (*) Expr "-" Factor ["+"]
    //     Expr = (*) Expr "-" Factor ["-"]
    //     Expr = (*) Factor [")"]
    //     Expr = (*) Factor ["+"]
    //     Expr = (*) Factor ["-"]
    //     Factor = (*) Factor "*" Term [")"]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["+"]
    //     Factor = (*) Factor "*" Term ["-"]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "/" Term [")"]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["+"]
    //     Factor = (*) Factor "/" Term ["-"]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Term [")"]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["+"]
    //     Factor = (*) Term ["-"]
    //     Factor = (*) Term ["/"]
    //     Term = (*) "(" Expr ")" [")"]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["+"]
    //     Term = (*) "(" Expr ")" ["-"]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = "(" (*) Expr ")" [")", "*", "+", "-", "/"]
    //     Term = (*) r#"[0-9]+"# [")"]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["+"]
    //     Term = (*) r#"[0-9]+"# ["-"]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //
    //   "(" -> S13
    //   r#"[0-9]+"# -> S14
    //
    //     Expr -> S24
    //     Factor -> S11
    //     Term -> S12
    pub fn __state13<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        let __sym0 = &mut Some(__sym0);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state13(input, __tokens, __sym1));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym1 = (__loc1, (__tok0), __loc2);
                __result = try!(__state14(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            if __sym0.is_none() {
                return Ok(__result);
            }
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Expr(__sym1) => {
                    __result = try!(__state24(input, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::Factor(__sym1) => {
                    __result = try!(__state11(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::Term(__sym1) => {
                    __result = try!(__state12(input, __tokens, __lookahead, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 14
    //     AllInputs = [r#"[0-9]+"#]
    //     OptionalInputs = []
    //     FixedInputs = [r#"[0-9]+"#]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Term)
    //
    //     Term = r#"[0-9]+"# (*) [")", "*", "+", "-", "/"]
    //
    //   [")", "*", "+", "-", "/"] -> Term = r#"[0-9]+"# => ActionFn(7);
    //
    pub fn __state14<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) => {
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7(input, __sym0);
                let __nt = __Nonterminal::Term((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 15
    //     AllInputs = [Expr, "+", Factor]
    //     OptionalInputs = [Expr, "+"]
    //     FixedInputs = [Factor]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Expr "+" Factor (*) ["+", "-", EOF]
    //     Factor = Factor (*) "*" Term ["*", "+", "-", "/", EOF]
    //     Factor = Factor (*) "/" Term ["*", "+", "-", "/", EOF]
    //
    //   "*" -> S8
    //   "/" -> S9
    //   ["+", "-", EOF] -> Expr = Expr, "+", Factor => ActionFn(2);
    //
    pub fn __state15<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, F, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state8(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((__loc1, (5, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state9(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action2(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 16
    //     AllInputs = [Expr, "-", Factor]
    //     OptionalInputs = [Expr, "-"]
    //     FixedInputs = [Factor]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Expr "-" Factor (*) ["+", "-", EOF]
    //     Factor = Factor (*) "*" Term ["*", "+", "-", "/", EOF]
    //     Factor = Factor (*) "/" Term ["*", "+", "-", "/", EOF]
    //
    //   "*" -> S8
    //   "/" -> S9
    //   ["+", "-", EOF] -> Expr = Expr, "-", Factor => ActionFn(1);
    //
    pub fn __state16<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, F, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state8(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((__loc1, (5, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state9(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action1(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 17
    //     AllInputs = [Factor, "*", Term]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "*", Term]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "*" Term (*) ["*", "+", "-", "/", EOF]
    //
    //   ["*", "+", "-", "/", EOF] -> Factor = Factor, "*", Term => ActionFn(4);
    //
    pub fn __state17<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) |
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action4(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Factor((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 18
    //     AllInputs = [Factor, "/", Term]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "/", Term]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "/" Term (*) ["*", "+", "-", "/", EOF]
    //
    //   ["*", "+", "-", "/", EOF] -> Factor = Factor, "/", Term => ActionFn(5);
    //
    pub fn __state18<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) |
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action5(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Factor((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 19
    //     AllInputs = ["(", Expr, ")"]
    //     OptionalInputs = []
    //     FixedInputs = ["(", Expr, ")"]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Term)
    //
    //     Term = "(" Expr ")" (*) ["*", "+", "-", "/", EOF]
    //
    //   ["*", "+", "-", "/", EOF] -> Term = "(", Expr, ")" => ActionFn(8);
    //
    pub fn __state19<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, &'input str, usize),
        __sym1: (usize, F, usize),
        __sym2: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) |
            None => {
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action8(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Term((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 20
    //     AllInputs = [Expr, "+"]
    //     OptionalInputs = []
    //     FixedInputs = [Expr, "+"]
    //     WillPushLen = 1
    //     WillPush = [Factor]
    //     WillProduce = Some(Expr)
    //
    //     Expr = Expr "+" (*) Factor [")", "+", "-"]
    //     Factor = (*) Factor "*" Term [")", "+", "-"]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "/" Term [")", "+", "-"]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Term [")", "+", "-"]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["/"]
    //     Term = (*) "(" Expr ")" [")", "+", "-"]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = (*) r#"[0-9]+"# [")", "+", "-"]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //
    //   "(" -> S13
    //   r#"[0-9]+"# -> S14
    //
    //     Factor -> S25
    //     Term -> S12
    pub fn __state20<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        let __sym0 = &mut Some(__sym0);
        let __sym1 = &mut Some(__sym1);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state13(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state14(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            if __sym1.is_none() {
                return Ok(__result);
            }
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Factor(__sym2) => {
                    __result = try!(__state25(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state12(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 21
    //     AllInputs = [Expr, "-"]
    //     OptionalInputs = []
    //     FixedInputs = [Expr, "-"]
    //     WillPushLen = 1
    //     WillPush = [Factor]
    //     WillProduce = Some(Expr)
    //
    //     Expr = Expr "-" (*) Factor [")", "+", "-"]
    //     Factor = (*) Factor "*" Term [")", "+", "-"]
    //     Factor = (*) Factor "*" Term ["*"]
    //     Factor = (*) Factor "*" Term ["/"]
    //     Factor = (*) Factor "/" Term [")", "+", "-"]
    //     Factor = (*) Factor "/" Term ["*"]
    //     Factor = (*) Factor "/" Term ["/"]
    //     Factor = (*) Term [")", "+", "-"]
    //     Factor = (*) Term ["*"]
    //     Factor = (*) Term ["/"]
    //     Term = (*) "(" Expr ")" [")", "+", "-"]
    //     Term = (*) "(" Expr ")" ["*"]
    //     Term = (*) "(" Expr ")" ["/"]
    //     Term = (*) r#"[0-9]+"# [")", "+", "-"]
    //     Term = (*) r#"[0-9]+"# ["*"]
    //     Term = (*) r#"[0-9]+"# ["/"]
    //
    //   "(" -> S13
    //   r#"[0-9]+"# -> S14
    //
    //     Factor -> S26
    //     Term -> S12
    pub fn __state21<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        let __sym0 = &mut Some(__sym0);
        let __sym1 = &mut Some(__sym1);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state13(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state14(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            if __sym1.is_none() {
                return Ok(__result);
            }
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Factor(__sym2) => {
                    __result = try!(__state26(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state12(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 22
    //     AllInputs = [Factor, "*"]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "*"]
    //     WillPushLen = 1
    //     WillPush = [Term]
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "*" (*) Term [")", "*", "+", "-", "/"]
    //     Term = (*) "(" Expr ")" [")", "*", "+", "-", "/"]
    //     Term = (*) r#"[0-9]+"# [")", "*", "+", "-", "/"]
    //
    //   "(" -> S13
    //   r#"[0-9]+"# -> S14
    //
    //     Term -> S27
    pub fn __state22<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state13(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state14(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state27(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                    return Ok(__result);
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 23
    //     AllInputs = [Factor, "/"]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "/"]
    //     WillPushLen = 1
    //     WillPush = [Term]
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "/" (*) Term [")", "*", "+", "-", "/"]
    //     Term = (*) "(" Expr ")" [")", "*", "+", "-", "/"]
    //     Term = (*) r#"[0-9]+"# [")", "*", "+", "-", "/"]
    //
    //   "(" -> S13
    //   r#"[0-9]+"# -> S14
    //
    //     Term -> S28
    pub fn __state23<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state13(input, __tokens, __sym2));
            }
            Some((__loc1, (6, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state14(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__sym2) => {
                    __result = try!(__state28(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                    return Ok(__result);
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    // State 24
    //     AllInputs = ["(", Expr]
    //     OptionalInputs = ["("]
    //     FixedInputs = [Expr]
    //     WillPushLen = 1
    //     WillPush = [")"]
    //     WillProduce = None
    //
    //     Expr = Expr (*) "+" Factor [")", "+", "-"]
    //     Expr = Expr (*) "-" Factor [")", "+", "-"]
    //     Term = "(" Expr (*) ")" [")", "*", "+", "-", "/"]
    //
    //   ")" -> S29
    //   "+" -> S20
    //   "-" -> S21
    //
    pub fn __state24<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                let __sym0 = __sym0.take().unwrap();
                __result = try!(__state29(input, __tokens, __sym0, __sym1, __sym2));
                return Ok(__result);
            }
            Some((__loc1, (3, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state20(input, __tokens, __sym1, __sym2));
                return Ok(__result);
            }
            Some((__loc1, (4, __tok0), __loc2)) => {
                let __sym2 = (__loc1, (__tok0), __loc2);
                __result = try!(__state21(input, __tokens, __sym1, __sym2));
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 25
    //     AllInputs = [Expr, "+", Factor]
    //     OptionalInputs = [Expr, "+"]
    //     FixedInputs = [Factor]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Expr "+" Factor (*) [")", "+", "-"]
    //     Factor = Factor (*) "*" Term [")", "*", "+", "-", "/"]
    //     Factor = Factor (*) "/" Term [")", "*", "+", "-", "/"]
    //
    //   "*" -> S22
    //   "/" -> S23
    //   [")", "+", "-"] -> Expr = Expr, "+", Factor => ActionFn(2);
    //
    pub fn __state25<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, F, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state22(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((__loc1, (5, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state23(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((_, (1, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action2(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 26
    //     AllInputs = [Expr, "-", Factor]
    //     OptionalInputs = [Expr, "-"]
    //     FixedInputs = [Factor]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = None
    //
    //     Expr = Expr "-" Factor (*) [")", "+", "-"]
    //     Factor = Factor (*) "*" Term [")", "*", "+", "-", "/"]
    //     Factor = Factor (*) "/" Term [")", "*", "+", "-", "/"]
    //
    //   "*" -> S22
    //   "/" -> S23
    //   [")", "+", "-"] -> Expr = Expr, "-", Factor => ActionFn(1);
    //
    pub fn __state26<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, F, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state22(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((__loc1, (5, __tok0), __loc2)) => {
                let __sym3 = (__loc1, (__tok0), __loc2);
                __result = try!(__state23(input, __tokens, __sym2, __sym3));
                return Ok(__result);
            }
            Some((_, (1, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action1(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Expr((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 27
    //     AllInputs = [Factor, "*", Term]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "*", Term]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "*" Term (*) [")", "*", "+", "-", "/"]
    //
    //   [")", "*", "+", "-", "/"] -> Factor = Factor, "*", Term => ActionFn(4);
    //
    pub fn __state27<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) => {
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action4(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Factor((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 28
    //     AllInputs = [Factor, "/", Term]
    //     OptionalInputs = []
    //     FixedInputs = [Factor, "/", Term]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Factor)
    //
    //     Factor = Factor "/" Term (*) [")", "*", "+", "-", "/"]
    //
    //   [")", "*", "+", "-", "/"] -> Factor = Factor, "/", Term => ActionFn(5);
    //
    pub fn __state28<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: (usize, F, usize),
        __sym1: (usize, &'input str, usize),
        __sym2: (usize, F, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) => {
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action5(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Factor((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 29
    //     AllInputs = ["(", Expr, ")"]
    //     OptionalInputs = []
    //     FixedInputs = ["(", Expr, ")"]
    //     WillPushLen = 0
    //     WillPush = []
    //     WillProduce = Some(Term)
    //
    //     Term = "(" Expr ")" (*) [")", "*", "+", "-", "/"]
    //
    //   [")", "*", "+", "-", "/"] -> Term = "(", Expr, ")" => ActionFn(8);
    //
    pub fn __state29<
        'input,
        F,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: (usize, &'input str, usize),
        __sym1: (usize, F, usize),
        __sym2: (usize, &'input str, usize),
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>), __ParseError<usize,(usize, &'input str),()>> where
      F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<F>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) |
            Some((_, (3, _), _)) |
            Some((_, (4, _), _)) |
            Some((_, (5, _), _)) => {
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action8(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::Term((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
                return Ok(__result);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }
}
pub use self::__parse__Expr::parse_Expr;
mod __intern_token {
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    pub struct __Matcher<'input> {
        text: &'input str,
        consumed: usize,
    }

    fn __tokenize(text: &str) -> Option<(usize, usize)> {
        let mut __chars = text.char_indices();
        let mut __current_match: Option<(usize, usize)> = None;
        let mut __current_state: usize = 0;
        loop {
            match __current_state {
                0 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        40 => /* '(' */ {
                            __current_match = Some((0, __index + 1));
                            __current_state = 1;
                            continue;
                        }
                        41 => /* ')' */ {
                            __current_match = Some((1, __index + 1));
                            __current_state = 2;
                            continue;
                        }
                        42 => /* '*' */ {
                            __current_match = Some((2, __index + 1));
                            __current_state = 3;
                            continue;
                        }
                        43 => /* '+' */ {
                            __current_match = Some((3, __index + 1));
                            __current_state = 4;
                            continue;
                        }
                        45 => /* '-' */ {
                            __current_match = Some((4, __index + 1));
                            __current_state = 5;
                            continue;
                        }
                        47 => /* '/' */ {
                            __current_match = Some((5, __index + 1));
                            __current_state = 6;
                            continue;
                        }
                        48 ... 57 => {
                            __current_match = Some((6, __index + __ch.len_utf8()));
                            __current_state = 7;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                1 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                2 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                3 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                4 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                5 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                6 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                7 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((6, __index + __ch.len_utf8()));
                            __current_state = 9;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                8 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                9 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((6, __index + __ch.len_utf8()));
                            __current_state = 9;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                _ => { panic!("invalid state {}", __current_state); }
            }
        }
    }

    impl<'input> __Matcher<'input> {
        pub fn new(s: &'input str) -> __Matcher<'input> {
            __Matcher { text: s, consumed: 0 }
        }
    }

    impl<'input> Iterator for __Matcher<'input> {
        type Item = Result<(usize, (usize, &'input str), usize), __ParseError<usize,(usize, &'input str),()>>;

        fn next(&mut self) -> Option<Self::Item> {
            let __text = self.text.trim_left();
            let __whitespace = self.text.len() - __text.len();
            let __start_offset = self.consumed + __whitespace;
            if __text.is_empty() {
                self.text = __text;
                self.consumed = __start_offset;
                None
            } else {
                match __tokenize(__text) {
                    Some((__index, __length)) => {
                        let __result = &__text[..__length];
                        let __remaining = &__text[__length..];
                        let __end_offset = __start_offset + __length;
                        self.text = __remaining;
                        self.consumed = __end_offset;
                        Some(Ok((__start_offset, (__index, __result), __end_offset)))
                    }
                    None => {
                        Some(Err(__ParseError::InvalidToken { location: __start_offset }))
                    }
                }
            }
        }
    }
}

pub fn __action0<
    'input,
    F,
>(
    input: &'input str,
    (_, __0, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    (__0)
}

pub fn __action1<
    'input,
    F,
>(
    input: &'input str,
    (_, l, _): (usize, F, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, r, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    l - r
}

pub fn __action2<
    'input,
    F,
>(
    input: &'input str,
    (_, l, _): (usize, F, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, r, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    l + r
}

pub fn __action3<
    'input,
    F,
>(
    input: &'input str,
    (_, __0, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    (__0)
}

pub fn __action4<
    'input,
    F,
>(
    input: &'input str,
    (_, l, _): (usize, F, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, r, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    l * r
}

pub fn __action5<
    'input,
    F,
>(
    input: &'input str,
    (_, l, _): (usize, F, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, r, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    l / r
}

pub fn __action6<
    'input,
    F,
>(
    input: &'input str,
    (_, __0, _): (usize, F, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    (__0)
}

pub fn __action7<
    'input,
    F,
>(
    input: &'input str,
    (_, n, _): (usize, &'input str, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    match n.parse() { Ok(v) => v, Err(_) => panic!("can't parse") }
}

pub fn __action8<
    'input,
    F,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, F, usize),
    (_, _, _): (usize, &'input str, usize),
) -> F where
  F: Debug + FromStr + Sub<Output=F> + Add<Output=F> + Mul<Output=F> + Div<Output=F>,
{
    (__0)
}

pub trait __ToTriple<'input, F, > {
    type Error;
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),Self::Error>;
}

impl<'input, F, > __ToTriple<'input, F, > for (usize, (usize, &'input str), usize) {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),()> {
        Ok(value)
    }
}
impl<'input, F, > __ToTriple<'input, F, > for Result<(usize, (usize, &'input str), usize),()> {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),()> {
        value
    }
}
