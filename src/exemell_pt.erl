% vim: cc=80 ft=erlang ts=2 sw=2 et
%
% Copyright 1992-2011 Matthias Horn. All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
% 
% 1. Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% 
% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT "AS IS" AND ANY EXPRESS OR
% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% The views and conclusions contained in the software and documentation are
% those of the authors and should not be interpreted as representing official
% policies, either expressed or implied, of Matthias Horn.

-module(exemell_pt).

-export([parse_transform/2]).

-ifndef(DEBUG).
-define(DEBUG,true).
-endif.

-include("debug.hrl").
-include_lib("exemell/include/exemell.hrl").
-include("parsetransform.hrl").



% -record(ns,{prefix = none :: binary() | none, uri = none :: nsuri()}).
% namespace({none,URI}) -> #ns{prefix=none,uri=asBin(URI)};
% namespace({Prefix,URI}) -> #ns{prefix=asBin(Prefix),uri=asBin(URI)};
% namespace(URI) -> namespace({none,URI}).

% -record(tag,{tag :: atom()}).
tag(Tag) -> asBin(Tag). % #tag{tag=asAtom(Tag)}.

-record(attribute,{name,getter,setter,rule,selector,mode}).
attribute({Name,Options}) -> 
  Attr0 = attribute_opts(Options,#attribute{name=asAtom(Name)}),
  Attr1 = case Attr0#attribute.rule of undefined -> Attr0#attribute{rule={'AND',[{asAtom(Name),1},2]}}; _ -> Attr0 end,
  Attr2 = case Attr1#attribute.getter of undefined -> Attr1#attribute{getter=[{tag,asBin(Name)},asFun(Name)]}; _ -> Attr1 end,
  Attr3 = case Attr2#attribute.setter of undefined -> Attr2#attribute{setter=asFun(Name)}; _ -> Attr2 end,
  Attr4 = case Attr3#attribute.selector of undefined -> Attr3#attribute{selector=2}; _ -> Attr3 end,
  Attr5 = case Attr4#attribute.mode of undefined -> Attr4#attribute{mode=singleton}; _ -> Attr4 end,
  Attr5;
attribute(Name) ->
  attribute({Name,[]}).
attribute_opts(singleton,Attr) -> Attr#attribute{mode=singleton};
attribute_opts(list,Attr) -> Attr#attribute{mode=list};
attribute_opts(accum,Attr) -> Attr#attribute{mode=accum};
attribute_opts({rule,Rule},Attr) -> Attr#attribute{rule=Rule};
attribute_opts({getter,Getter},Attr) -> Attr#attribute{getter=Getter};
attribute_opts({setter,Setter},Attr) -> Attr#attribute{setter=Setter};
attribute_opts({selector,SEL},Attr) -> Attr#attribute{selector=SEL};
attribute_opts(notag,Attr) -> attribute_opts({getter,getter(Attr)},Attr);
attribute_opts({tag,Tag},Attr) -> attribute_opts({getter,[{tag,Tag},getter(Attr)]},Attr);
attribute_opts({element,N},Attr) -> attribute_opts({selector,N},Attr);
attribute_opts([],Attr) -> Attr;
attribute_opts([H|T],Attr) -> attribute_opts(T,attribute_opts(H,Attr));
attribute_opts(A,Options) when is_atom(A) -> attribute_opts({A,true},Options);
attribute_opts(_,Attr) -> Attr.

-record(child,{name,setter,getter,mode,rule,selector,extern}).
child({Name,Options}) -> children({Name,[singleton]++Options});
child(Name) -> child({Name,[]}).
children({Name,Options}) ->
  Child0 = child_opts(Options,#child{name=asAtom(Name)}),
  Child1 = case Child0#child.rule of undefined -> Child0#child{rule={asAtom(Name),1}}; _ -> Child0 end,
  Child2 = case Child1#child.getter of undefined -> Child1#child{getter=asFun(Name)}; _ -> Child1 end,
  Child3 = case Child2#child.setter of undefined -> Child2#child{setter=asFun(Name)}; _ -> Child2 end,
  Child4 = case Child3#child.selector of undefined -> Child3#child{selector=[]}; _ -> Child3 end,
  Child5 = case Child4#child.extern of undefined -> Child4#child{extern={exemellp,xml}}; _ -> Child4 end,
  Child6 = case Child5#child.mode of undefined -> Child5#child{mode=list}; _ -> Child5 end,
  Child6;
children(Name) -> children({Name,[]}).
child_opts(singleton,Child) -> Child#child{mode=singleton};
child_opts(list,Child) -> Child#child{mode=list};
child_opts(accum,Child) -> Child#child{mode=accum};
child_opts(notag,Child) -> child_opts({getter,getter(Child)},Child);
child_opts({tag,Tag},Child) -> child_opts({gettern,[{tag,Tag},getter(Child)]},Child);
child_opts({rule,Rule},Child) -> Child#child{rule=Rule};
child_opts({getter,Getter},Child) -> Child#child{getter=(Getter)};
child_opts({setter,Setter},Child) -> Child#child{setter=(Setter)};
child_opts({module,Module},Child) -> child_opts({extern,{Module,'xml'}},Child);
child_opts({extern,EXTERN},Child) -> Child#child{extern=(EXTERN)};
child_opts({selector,SEL},Child) -> Child#child{selector=SEL};
child_opts({element,N},Child) -> child_opts({selector,[N]},Child);
child_opts([],Child) -> Child;
child_opts([H|T],Child) -> child_opts(T,child_opts(H,Child));
child_opts(A,Options) when is_atom(A) -> child_opts({A,true},Options);
child_opts(_,Child) -> Child.

-record(inner_xml,{name,setter,getter}).
inner_xml({Name,Options}) -> inner_xml_opts([{setter,inner_xml},{getter,inner_xml},{tag,Name}]++Options,#inner_xml{name=asAtom(Name)});
inner_xml(Name) -> inner_xml({Name,[]}).
inner_xml_opts({getter,Getter},InnerXML) -> InnerXML#inner_xml{getter=(Getter)};
inner_xml_opts({setter,Setter},InnerXML) -> InnerXML#inner_xml{setter=(Setter)};
inner_xml_opts([],InnerXML) -> InnerXML;
inner_xml_opts([H|T],InnerXML) -> inner_xml_opts(T,inner_xml_opts(H,InnerXML));
inner_xml_opts(A,Options) when is_atom(A) -> inner_xml_opts({A,true},Options);
inner_xml_opts(_,InnerXML) -> InnerXML.

-record(block,{name,module,rule='_'}).
block({Name,Options}) -> block_opts([{module,exemell_block},{rule,Name},block]++Options,#block{name=asAtom(Name)});
block(Name) -> block({Name,[]}).
block_opts({module,Module},Block) -> Block#block{module=asAtom(Module)};
block_opts({rule,Rule},Block) -> Block#block{rule=Rule};
block_opts([],Block) -> Block;
block_opts([H|T],Block) -> block_opts(T,block_opts(H,Block));
block_opts(A,Options) when is_atom(A) -> block_opts({A,true},Options);
block_opts(_,Block) -> Block.


-record(xml,{exemell_namespace=false,exemell_namespace_attributes=false,exemell_block=false,exemell_blob=false,generate_namespace=false}).
xml_opts(Options) -> xml_opts(Options,#xml{}).
xml_opts({exemell_namespace,Module},Options) -> Options#xml{exemell_namespace=Module};
xml_opts({exemell_namespace_attributes,Module},Options) -> Options#xml{exemell_namespace_attributes=Module};
xml_opts({exemell_block,Module},Options) -> Options#xml{exemell_block=Module};
xml_opts({exemell_blob,Module},Options) -> Options#xml{exemell_blob=Module};
xml_opts({generate_namespace,Module},Options) -> Options#xml{generate_namespace=Module};
xml_opts([],Options) -> Options;
xml_opts([H|T],Options) -> xml_opts(T,xml_opts(H,Options));
xml_opts(A,Options) when is_atom(A) -> xml_opts({A,true},Options);
xml_opts(_,Options) -> Options.






parse_transform(Forms0,_Options) ->
  
  Forms1 = erl_syntax:form_list(lists:flatten([Forms0])),
  % Forms1 = Forms0,
  Module = getModule(Forms1),
  Namespace = getNamespace(Forms1),
  Tag = getTag(Forms1),
  Attributes = getAttributes(Forms1),
  Blocks0 = getBlocks(Forms1),
  Children = getChildren(Forms1),
  InnerXML = getInnerXML(Forms1),
  Opts = getXMLOpts(Forms1),
  Blocks = lists:flatten(case Opts#xml.generate_namespace of __A1 when __A1=/=true -> Blocks0; _ ->
    Blocks0++findBlocks(Module)
  end),
  ?debug(Module),
  ?debug(Namespace),
  ?debug(Tag),
  NSTag = case Namespace of
    none -> ?abstract(Tag);
    _ -> ?abstract({Namespace,Tag})
  end,
  ?debug(Attributes),
  ?debug(Children),
  ?debug(InnerXML),
  ?debug(Blocks),
  ?debug(Opts#xml.generate_namespace),
  ?debug_pt(Forms1),
  Forms2 = case Opts#xml.exemell_namespace of __A2 when __A2=/=true -> Forms1; _ ->
    Forms21 = case Opts#xml.exemell_namespace_attributes of __A5 when __A5=/=true -> Forms1; _ ->
      insert_at_end(insert_in_head(Forms1,?export(xml_attribute,4)),
        ?function(xml_attribute,
          [ ?clause([?atom(none),?var('Name'),?var('Value'),?var('State')], none,
              [?tuple([?tuple([?var('Name'),?var('Value')]),?var('State')])])
          , ?clause([?var('NS'),?var('Name'),?var('Value'),?var('State')], none,
              [?tuple([?tuple([?tuple([?var('NS'),?var('Name')]),?var('Value')]),?var('State')])])
          ]))
    end,
    insert_at_end(insert_in_head(Forms21,?export(xml_block,4)),
      ?function(xml_block,
        [ ?clause([?var('NS'),?var('Tag'),?var('Attrs'),?var('State')], none, [
          ?ifs([?clause([],[rule_code(B#block.rule,?var('Tag'))],
                [?apply(B#block.module,xml_block,[?var('NS'),?var('Tag'),?var('Attrs'),?var('State')])])
        || B <- Blocks ]
        )])]))
  end,
  ?debug_pt(Forms2),
  Forms3 = case Opts#xml.exemell_blob of __A4 when __A4=/=true -> Forms2; _ ->
    Forms30 = Forms2,
    Forms31 = case getFunction(xml_blob,3,Forms30) of __A8 when __A8=/=undefined -> Forms30; _ ->
    insert_at_end(insert_in_head(Forms2,[?export(xml_blob,3)]),
      ?function(xml_blob,
        [ ?clause([?var('Body'),?var('Self'),?var('State')],none,
            [setter_code(InnerXML,?var('Body'),?var('Self'))])
        ])) end,
    Forms32 = case getFunction(xml_block,4,Forms31) of __A12 when __A12=/=undefined -> Forms31; _ ->
    insert_at_end(insert_in_head(Forms31,[?export(xml_block,4)]),
      ?function(xml_block,
        [ ?clause([?var('_NS'),?var('_Tag'),?var('Attrs'),?var('PARSER')],none,
            [ ?tuple([?atom(blob),?atom(Module)
                     ,?apply('xml_block#attributes',[?var('Attrs')])
                     ,?var('PARSER')]) ]
            )
        ])) end,
    Forms33 = case getFunction(xml,2,Forms32) of __A11 when __A11=/=undefined -> Forms32; _ ->
      insert_at_end(insert_in_head(Forms32,[?export(xml,2)]),
      ?function(xml,
        [ ?clause([?var('Printer'),?var('Self')],none,
             [?apply(exemellp,xml,[NSTag,?apply('xml#attributes',[?var('Self')]), ?list([?tuple([?atom(raw),getter_code(InnerXML,?var('Self'))])]),?var('Printer')])]
          )]))
      end,
    Forms33

  end,
  ?debug_pt(Forms3),
  Forms4 = case Opts#xml.exemell_block of __A3 when __A3=/=true -> Forms3; _ ->
    Buckets = [C || C=#child{mode=list} <- Children],
    BucketsV = [var() || _<-Buckets],
    Forms40 = Forms3,
    Forms41 = case {getFunction(xml_child,3,Forms40),getFunction(xml_end,2,Forms3)} of __A9 when __A9=/={undefined,undefined} -> Forms40; _ ->
    insert_at_end(insert_in_head(Forms40,[?export(xml_child,3),?export(xml_end,2)]),
    [ ?function(xml_child, 
        [ ?clause([?var('Child'),?tuple([?atom('#'),?var('Self')]++BucketsV),?var('State')],none,
            [?tuple([ ?ifs([?clause([],[rule_code(C#child.rule,?var('Child'))],
              [ case C#child.mode of
                  list ->
                    ?tuple([?atom('#'),?var('Self')]++[ case B of
                                                          C -> ?list([selector_code(C,?var('Child'))],V);
                                                          _ -> V
                                                        end || {B,V} <- lists:zip(Buckets,BucketsV) ]);
                  accum ->
                    ?tuple([?atom('#'),setter_code(C,selector_code(C,?var('Child')),?var('Self'))]++BucketsV);
                  singleton ->
                    ?tuple([?atom('#'),setter_code(C,selector_code(C,?var('Child')),?var('Self'))]++BucketsV)
                end ])
              || C <- Children]) , ?var('State')])])
        ]
      )
    , ?function(xml_end,
        [ ?clause([?tuple([?atom('#'),?var('Self')]++BucketsV),?var('State')],none,
            [ ?tuple([
              lists:foldl(fun ({B,V},Self) -> setter_code(B,?apply(lists,reverse,[V]),Self) end,?var('Self'),lists:zip(Buckets,BucketsV))
              ,?var('State')]) ]
          )
        ])
    ]) end,
  ?debug_pt(Forms41),
    Forms42 = case getFunction(xml_block,4,Forms41) of __A13 when __A13=/=undefined -> Forms41; _ ->
    insert_at_end(insert_in_head(Forms41,[?export(xml_block,4)]),
      ?function(xml_block,
        [ ?clause([?var('_NS'),?var('_Tag'),?var('Attrs'),?var('PARSER')],none,
            [ ?tuple([?atom(children),?atom(Module)
                     ,?tuple([?atom('#')
                             ,?apply('xml_block#attributes',[?var('Attrs')])
                             ]++[?nil || _<-Buckets])
                     ,?var('PARSER')]) ]
            )
        ])) end,
  ?debug_pt(Forms42),
    Forms43 = case getFunction(xml,2,Forms42) of __A14 when __A14=/=undefined -> Forms42; _ ->
      insert_at_end(insert_in_head(Forms42,[?export(xml,2)]),
      ?function(xml,
        [ ?clause([?var('Printer'),?var('Self')],none,
             [?apply(exemellp,xml,[NSTag,?apply('xml#attributes',[?var('Self')]), 
              ?cases(appends([ case C#child.mode of
                                 list -> getter_code(C,?var('Self'));
                                 accum -> getter_code(C,?var('Self'));
                                 singleton -> ?cases(getter_code(C,?var('Self')),[?clause([?atom(undefined)],none,[?nil]),begin NVar2=var(), ?clause([NVar2],none,[?list([NVar2])]) end])
                               end || C <- Children ]),[?clause([?nil],none,[?atom(none)]),?clause([?var('Children')],none,[?var('Children')])])
              
              ,?var('Printer')])]
          )]))
      end,
    Forms43
  end,
  ?debug_pt(Forms4),
  Forms5 = case Opts#xml.exemell_block or Opts#xml.exemell_blob of __A15 when __A15=/=true -> Forms4; _ ->
    Auckets = [C || C=#attribute{mode=list} <- Attributes],
    AucketsV = [var() || _<-Auckets],
    Forms50 = Forms4,
    Forms51 = case getFunction('xml_block#attributes',2,Forms50) of __A7 when __A7=/=undefined -> ?debug(__A7), Forms50; _ ->
    insert_at_end(Forms50,
      ?function('xml_block#attributes',
        [ ?clause([?list([?var('Attr')],?var('Attrs')),?tuple([?atom('#'),?var('Self')]++AucketsV)],none,[
            ?apply('xml_block#attributes',[?var('Attrs'),
              ?ifs([?clause([],rule_code(A#attribute.rule,?var('Attr')),[
                case A#attribute.mode of
                  list ->
                    ?tuple([?atom('#'),?var('Self')]++[ case B of
                                                          A -> ?list([selector_code(A,?var('Attr'))],V);
                                                          _ -> V
                                                        end || {B,V} <- lists:zip(Auckets,AucketsV) ]);
                  accum ->
                    ?tuple([?atom('#'),setter_code(A,selector_code(A,?var('Attr')),?var('Self'))]++AucketsV);
                  singleton ->
                    ?tuple([?atom('#'),setter_code(A,selector_code(A,?var('Attr')),?var('Self'))]++AucketsV)
                end ]) || A<-Attributes
                   ] ++ case Attributes of [] -> [?clause([],?atom(false),[?atom(never)])]; _ -> [] end)
            ])
          ])
        , ?clause([?nil,?tuple([?atom('#'),?var('Self')]++AucketsV)],none,[ lists:foldl(fun ({A,V},Self) -> setter_code(A,?apply(lists,reverse,[V]),Self) end,?var('Self'),lists:zip(Auckets,AucketsV)) ]) %% TODO!
        ])) end,
    Forms52 = case getFunction('xml_block#attributes',1,Forms50) of __A16 when __A16=/=undefined -> ?debug(__A16), Forms50; _ ->
    insert_at_end(Forms51,
      ?function('xml_block#attributes', [ ?clause([?var('Attrs')],none,[?apply('xml_block#attributes',[?var('Attrs'),?tuple([?atom('#'),?apply(init,[])]++[?nil || _<-Auckets])]) ])])) end,
    Forms53 = case getFunction('xml#attributes',1,Forms52) of __A17 when __A17=/=undefined -> ?debug(__A17), Forms52; _ ->
    insert_at_end(Forms52,
      ?function('xml#attributes', [ ?clause([?var('Self')],none,[
              appends([ case A#attribute.mode of
                          accum -> getter_code(A,?var('Self'));
                          list -> getter_code(A,?var('Self'));
                          singleton -> ?cases(getter_code(A,?var('Self')),[?clause([?atom(undefined)],none,[?nil]),begin NVar1=var(), ?clause([NVar1],none,[?list([NVar1])]) end])
                        end || A<-Attributes ])
      ])])) end,
    Forms53
  end,
  Forms6 = case getFunction(xml_prefix,0,Forms5) of __A19 when __A19=/=undefined -> Forms5; _ ->
    insert_at_end(insert_in_head(Forms5,?export(xml_prefix,0)), ?function(xml_prefix,[?clause([],[],[?atom(none)])])) end,
  ?debug_pt(Forms6),
  erl_syntax:revert_forms(Forms6).

appends([]) -> ?nil;
appends([H]) -> H;
appends([H|T]) -> ?infix(H,'++',appends(T)).

def([]) -> undefined;
def([undefined|A]) -> def(A);
def([A|_]) -> A.
def(A,B) -> def([A,B]).
setter(#child{name=Name,setter=Setter}) -> def(Setter,Name);
setter(#attribute{name=Name,setter=Setter}) -> def(Setter,Name);
setter(#inner_xml{name=Name,setter=Setter}) -> def(Setter,Name).
getter(#child{name=Name,getter=Getter}) -> def(Getter,Name);
getter(#attribute{name=Name,getter=Getter}) -> def(Getter,Name);
getter(#inner_xml{name=Name,getter=Getter}) -> def(Getter,Name).
selector(#child{selector=Selector}) -> def(Selector,[]);
selector(#attribute{selector=Selector}) -> def(Selector,[]);
selector(_) -> [].

selector_code(Object,Value) -> apply_code(selector(Object),[Value]).
setter_code(Object,Value,Self) ->
  apply_code(setter(Object),[Value,Self]).
getter_code(Object,Self) ->
  apply_code(getter(Object),[Self]).
apply_code([],[Argument]) -> Argument;
apply_code([H|T],Arguments) -> apply_code(H,[apply_code(T,Arguments)]);
apply_code({tag,Tag},[Argument]) when is_list(Tag) -> ?cases(Argument,[?clause([?atom(undefined)],none,[?atom(undefined)]),begin NVar=var(), ?clause([NVar],none,[?tuple([?abstract(T) || T<-Tag]++[NVar])]) end]);
apply_code({tag,Tag},Arguments) when is_list(Tag) -> ?tuple([?abstract(T) || T<-Tag]++Arguments);
apply_code({tag,Tag},Arguments) -> apply_code({tag,[Tag]},Arguments);
apply_code({Module,Function},Arguments) -> ?apply(Module,Function,Arguments);
apply_code(N,[Argument]) when is_integer(N) -> ?apply(element,[?int(N),Argument]);
apply_code(Function,Arguments) when is_atom(Function) -> ?apply(Function,Arguments).


%% Probably shouldn't use this on a live system as it will attempt to load every module in the code path...
findBlocks(URI) ->
  lists:flatten([ case containsNS(URI,Module:module_info(attributes)) of
      true -> 
        Tag = asAtom(?debug(proplists:get_value(tag,Module:module_info(attributes),asBin(Module)))),
        ?debug(Tag),
        case erlang:function_exported(Module,xml_blob,3) of
          true -> #block{name=Module,rule=Tag,module=Module};
          _ ->
            case erlang:function_exported(Module,xml_child,3) andalso erlang:function_exported(Module,xml_end,2) of
              true -> #block{name=Module,rule=Tag,module=Module};
              _ -> []
            end
        end;
      _ -> []
      % _ -> ?debug(Module), ?debug(Module:module_info(attributes)),[]
    end || Module <- allModules() ]).

containsNS(NS,{xmlns,[NS]}) -> true;
containsNS(NS,[H|T]) ->
  case containsNS(NS,H) of
    true -> true;
    _ -> containsNS(NS,T)
  end;
containsNS(_NS,_) -> false.

% Shouldn't use this on a live system as it will attempt to load every module in the code path...
% This is a bit crazy, but for code that is NEVER run in a live system it should be fine...
% NB: this code may require two compilations after a clean
allModules() ->
  loadAllModules(),
  erlang:loaded().
loadAllModules() ->
  lists:foreach(fun (Path) ->
    loadAllModules(Path)
    end,code:get_path()).
loadAllModules(Path) -> loadAllModules(Path++"/",Path).
loadAllModules(Root,Path) ->
  case file:list_dir(Path) of
    {ok,Files} -> lists:foreach(fun (File) -> loadAllModules(Root,Path++"/"++File) end,Files);
    _ -> 
      case lists:suffix(".beam",Path) of
        true -> case proplists:get_value(module,beam_lib:info(Path)) of
                  false -> ok;
                  Module -> code:ensure_loaded(Module)
                end;
        _ -> ok
      end
  end.
    


asBin(A) -> asBin(A,<<>>).
asBin(A,Accum) when is_integer(A) -> <<Accum/bytes,A/utf8>>;
asBin(A,Accum) when is_atom(A) -> <<Accum/bytes,(atom_to_binary(A,utf8))/bytes>>;
asBin(A,Accum) when is_binary(A) -> <<Accum/bytes,A/bytes>>;
asBin([],Accum) -> Accum;
asBin([A|B],Accum) -> asBin(B,asBin(A,Accum)).
asAtom(A) when is_atom(A) -> A;
asAtom(A) -> binary_to_atom(asBin(A),utf8).
asFun({A,B}) -> {asAtom(A),asAtom(B)};
asFun(A) -> asAtom(A).


-type rule() :: [rule()] | '_' | '*' | 'CDATA' | {'AND',[rule()]} | {'OR',[rule()]} | {'NOT',rule()} | atom() | non_neg_integer().
%% Generates guards based on the rule definitions
%% we don't do any real optimisations here on the assumption that the compiler will do a decent job.
-spec rule_code(rule(),erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
rule_code('_',_Value) -> ?atom(true);
rule_code('*',Value) -> ?nif_is_tuple(Value);
rule_code('CDATA',Value) -> ?nif_not(?nif_is_tuple(Value));
rule_code({'AND',[]},_Value) -> ?atom(true);
rule_code({'AND',[Rule]},Value) -> rule_code(Rule,Value);
rule_code({'AND',[R1|RuleS]},Value) -> lists:foldl(fun (R,Accum) -> ?ANDALSO(Accum,rule_code(R,Value)) end,rule_code(R1,Value),RuleS);
rule_code({'OR',[]},_Value) -> ?atom(false);
rule_code({'OR',[Rule]},Value) -> rule_code(Rule,Value);
rule_code({'OR',[R1|RuleS]},Value) -> lists:foldl(fun (R,Accum) -> ?ORELSE(Accum,rule_code(R,Value)) end,rule_code(R1,Value),RuleS);
rule_code({'NOT',Rule},Value) -> ?nif_not(rule_code(Rule,Value));
rule_code({Rule,N},Value) when is_integer(N), N>0 -> ?ANDALSO(?nif_is_tuple(Value),rule_code(Rule,?nif_element(N,Value)));
rule_code(N,Value) when is_integer(N), N>=0 -> ?ANDALSO(?nif_is_tuple(Value),?eq(?nif_size(Value),?int(N)));
rule_code(RuleS,Value) when is_list(RuleS) -> rule_code({'OR',RuleS},Value);
rule_code(Rule,Value) -> ?eq(Value,?abstract(Rule)).


getModule(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {module,A} -> Accum++[A];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  case erl_syntax_lib:fold(Function,[],Forms) of
    [Module] -> Module
  end.

getNamespace(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {xmlns,{xmlns,A}} -> Accum++[A];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  case erl_syntax_lib:fold(Function,[],Forms) of
    [NS] -> NS;
    _ -> none
  end.

getTag(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {tag,{tag,A}} -> Accum++[tag(A)];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  case erl_syntax_lib:fold(Function,[],Forms) of
    [Tag] -> Tag;
    _ -> asBin(getModule(Forms))
  end.

getAttributes(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {attribute,{attribute,A}} -> Accum++[attribute(A)];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  erl_syntax_lib:fold(Function,[],Forms).

getChildren(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {child,{child,A}} -> Accum++[child(A)];
                {children,{children,A}} -> Accum++[children(A)];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  erl_syntax_lib:fold(Function,[],Forms).

getInnerXML(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {inner_xml,{inner_xml,A}} -> Accum++[inner_xml(A)];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  case erl_syntax_lib:fold(Function,[],Forms)  of
    [Result] -> Result;
    _ -> undefined
  end.

getBlocks(Forms) -> 
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {block,{block,A}} -> Accum++[block(A)];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  erl_syntax_lib:fold(Function,[],Forms).

getXMLOpts(Forms) -> 
 Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(F) of
                {xml_opts,{xml_opts,A}} when is_list(A) -> Accum++A;
                {xml_opts,{xml_opts,A}} -> Accum++[A];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  Result = erl_syntax_lib:fold(Function,[],Forms),
  xml_opts(
    case getChildren(Forms) of
      [] -> []; _ -> [exemell_block]
    end ++
    case getInnerXML(Forms) of
      undefined -> []; _ -> [exemell_blob]
    end ++
    case getBlocks(Forms) of
      [] -> []; _ -> [exemell_namespace]
    end ++
    case getFunction(xml_attribute,4,Forms) of
      undefined -> [exemell_namespace_attributes]; _ -> []
    end ++
    Result
  ).

getFunction(Name,Arity,Forms) ->
  Function =
    fun (F,Accum) ->
          case erl_syntax:type(F) of
            function ->
              case erl_syntax_lib:analyze_function(F) of
                {Name,Arity} -> Accum ++ [F];
                _ -> Accum
              end;
            _ -> Accum
          end
    end,
  case erl_syntax_lib:fold(Function,[],Forms) of
    [Fun] -> Fun;
    _ -> undefined
  end.
  



