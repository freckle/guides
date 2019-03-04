# API Style Guide

## Casing

- Object fields should be `camelCase`
- Enum identifiers should be `snake_case`

bad

    { foo: "MyCoolApiType"
    , bar_type: "chilledOutBar"
    }

good

    { foo: "my_cool_api_type"
    , barId: "chilled_out_bar"
    }

## Sum type encoding

- Constructor tag should be under the key `tag`
- Type contents should be under the key `contents`

bad 

    { type: "ice_cream"
    , details:
      { flavor: "vanilla_bean"
      }
    }

good

    { tag: "ice_cream"
    , contents:
      { flavor: "vanilla_bean"
      }
    }
