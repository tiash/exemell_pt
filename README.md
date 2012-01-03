Value Parsetransform
====================

This parse transform makes it easy to implement exemell behaviours.

The generated code is Parametric module compatible, and can be used in combination with `value_pt`.


The generated code is likely not suitable for persistance, I would advise using some form of serialisation.

Usage
-----
###Implement `exemell_block`
1. add `-compile([{parse_transform,exemell_pt}])` to your code.
2. add `-xmlns(NSModule)`
3. add `-tag(Tag)`. (optional, default to `-tag(?MODULE)`)
4. add `-attribute({Name,Opts})` declarations for all the attributes you will handle.
   Note that unhandled attributes will be discarded, you can however use a wild rule if this helps.
5. add `-child({Name,Opts})` and `-children({Name,Opts})` declarations for all the children you intend to handle.
   Unhandled children will be discarded, you can however use a wild rule.
6. add `-xml_opts(Opts)` if necessary.

###Implement `exemell_blob`
1. add `-compile([{parse_transform,exemell_pt}])` to your code.
2. add `-namespace(URI)` or `-namespace({PREFIX,URI})`.
3. add `-tag(Tag)`.
4. add `-attribute({Name,Opts})` declarations for all the attributes you will handle.
   Note that unhandled attributes will be discarded, you can however use a wild Rule for Name.
5. add one `-inner_xml({Name,Opts})` declaration to consume that modules code.
6. add `-xml_opts(Opts)` if necessary.

###Generate/Implement `exemell_namespace`
1. add `-compile([{parse_transform,exemell_pt}])` to your code.
2. define a function `xmlns() -> <<URI>>`.
3. Optionally, add a function `xml_prefix() -> <<Prefix>>`.
4. add `-block({Module,Opts})` declarations for every tag you wish to include; and or
5. add `-xml_opt(generate_namespace)` which will scan the code path for all relevant modules (may require repeated compilation).

Rules
-----
Rules are simple lists of atoms that will be used when selecting attributes/children/tags.
The rule the atom `'_'` is used to indicate that ANY term will match.
When matching attributes and children we also accept the rule `{RULE,N}` which will match iff `RULE` matches the `N`th element of the tuple.
When matching children we also accept the rules `'CDATA'` which will match anything except than tuples,
and `'*'` which will only match tuples.
The rule `{'AND',RULEs}` and `{'OR',RULEs}` will respecitively match iff all/any of `RULEs` match, lists are also treated as or rules.

Attributes
----------
`-attribute({Name,Opts})` declarations will generate a function `xml_intern#attributes(ATTRIBUTES,SELF,PARSER)->{SELF,PARSER}`, that will consume the attributes in order applying the rules, and `xml_extern#attributes(XMLSTATE,SELF)->IOLIS`. The function will also accept `undefined` in place of `SELF` and will use `init/0` to get a fresh self.

`-attribute(Name)` is short for `-attribtue({Name,[]})`.

###Options
* `{setter,Setter}` and `{getter,Getter}` function to use as getters and setters, defaults to `Name`.

* `{rule,Rule}` used to match the attributes, if ommited will default to `{Name,1}`.

* `{element,N}` only keep the `N`th tuple element, optional, no default.

* `{module,Module}` short form `{function,{Module,xml_attribute}}`.

* `{function,Function}` used when externalising by calling `Function(Value)` (if Function is 2-tuple, the first element is the module the second the function).

* `{name,Name}` used when externalising (if module was not given) by generating `Name=Value`,

* `{namespace,Prefix,Uri}` used when externalising (if module was not given) will add the required xmlns attribute and then prefix the attribute as appropriate.

Children
--------
`-child({Name,Opts})` and `-children({Name,Opts})` declarations will generate a function
  `xml_child(CHILD,{'#',STATE,SELF},PARSER)->{{'#',STATE,SELF},PARSER}`, that will consume the single child using Rules to select the appropriate output, and
  `xml_end({'#',STATE,SELF},PARSER)->{SELF,PARSER}` that will be called last to cleanup.
  Both functions will also accept `undefined` and `SELF` creating a fresh state and if necisarry calling `init/0` to get a new `SELF`.
  
  If `-block(...)` declarations where not given the code will also generate
    xml_block(_NS,_TAG,ATTRS,PARSER) -> {children,?MODULE,xml_attributes(ATTRS,undefined),PARSER}.

`-child(Name)` is short for `-child({Name,[]})`

`-children(Name)` is short for `-children({Name,[]})`

###Options
* `{setter,Setter}` and `{getter,Getter}` function to use as getters and setters, defaults to `Name`.

* `{rule,Rule}` used to match the attributes, if ommited will default to `{Name,1}`.

* `{element,N}` only keep the `N`th tuple element, optional, no default.

* `{module,Module}` short form `{function,{Module,xml_attribute}}`.

* `{function,Function}` used when externalising by calling `Function(Value)` (if Function is 2-tuple, the first element is the module the second the function), defaults to `{function,{exemell,xml_block}}`.

Blocks (Namespaces)
-------------------
`-block({Name,Opts})` declarations will generate a function  `xml_block(_NS,Name,ATTRS,PARSER) -> ...`

###Options

* `{rule,Rule}` used to match the tag, if ommited will default to `Name`.

* `{element,N}` only keep the `N`th tuple element, optional, no default.

* `{module,Module}` module containing an `xml_block/4` function, defaults to `Name`.
  This can be abused to 'include' other namespaces, output will still rever the original namespace.




     

