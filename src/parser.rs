use crate::errors::*;
use crate::lexer::*;
use std::fmt::{Display, Formatter};

pub fn parse<'a, I>(input: I) -> Result<Ast, Error>
where
    I: IntoIterator<Item = Result<Token<'a>, Error>>,
{
    for result in input {
        match result {
            Err(err) => {
                return Err(err);
            }
            Ok(_token) => {
                //todo!()
            }
        }
    }

    Ok(Ast {})
}

pub struct Ast {}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", "true")
    }
}
