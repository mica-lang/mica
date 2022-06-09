use mica_hl::language::value::Dict;
use mica_hl::TypeBuilder;

pub(super) fn define(builder: TypeBuilder<Dict>) -> TypeBuilder<Dict> {
   builder
      .add_function("len", Dict::len)
      .add_function("is_empty", Dict::is_empty)
      .add_function("insert", Dict::insert)
      .add_function("remove", Dict::remove)
      .add_function("get", Dict::get)
      .add_function("contains_key", Dict::contains_key)
      .add_function("clone", Dict::clone)
}
