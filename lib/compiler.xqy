(:
  XQuery Generator for mustache
:)
xquery version "1.0" ;
module namespace compiler = "compiler.xq" ;

declare function compiler:compile( $parseTree, $json ) {
 let $div := parse-xml( fn:concat( '&lt;div&gt;',
   fn:string-join(compiler:compile-xpath( $parseTree, json:parse( $json ) ), ''),  '&lt;/div&gt;') )
(: let $_ := compiler:log(("#### OUTPUT ", $div)) :)
 return compiler:handle-escaping($div) } ;

declare function  compiler:compile-xpath( $parseTree, $json ) {
 (: let $_ := compiler:log(("GOT", $parseTree, "WITH", serialize($json, map {'method': 'json'}),"")) return :) 
  compiler:compile-xpath( $parseTree, $json, 1, '' )
}; 

declare function compiler:compile-xpath( $parseTree, $json, $pos, $xpath ) { 
  for $node in $parseTree/node() 
(: let $_ := compiler:log(("~~ NOW COMPILING NODE", serialize($node), "AT", $pos, "WITH XPATH", $xpath, "SUBSTITUTION WAS", compiler:compile-node( $node, $json, $pos, $xpath ))) :)
  return compiler:compile-node( $node, $json, $pos, $xpath ) } ;

declare function compiler:compile-node( $node, $json, $pos, $xpath ) {
  typeswitch($node)
    case element(etag)    return
(: let $_ := compiler:log(("FINAL STEP ON ETAG", serialize($node), "XPATH", $xpath, "POS", $pos)) return :)
    compiler:eval( $node/@name, $json, $pos, $xpath )
    case element(utag)    return
(: let $_ := compiler:log(("FINAL STEP ON UTAG", serialize($node), "XPATH", $xpath, "POS", $pos)) return :)
    compiler:eval( $node/@name, $json, $pos, $xpath, fn:false() )
    case element(rtag)    return 
      fn:string-join(compiler:eval( $node/@name, $json, $pos, $xpath, fn:true(), 'desc' ), " ")
    case element(static)  return $node /fn:string()
    case element(comment) return ()
    case element(inverted-section) return
      let $sNode := compiler:unpath( fn:string( $node/@name ) , $json, $pos, $xpath )
(: let $_ := compiler:log(("IN AN INVERTED-SECTION ABOUT TO PROCESS", $sNode, "FOR", $node/@name)) :)
      return 
        if ( $sNode/@type = "boolean" and lower-case($sNode/text()) = 'true' ) 
        then ()
        else if ($sNode/@type = "array")
             then if (fn:exists($sNode/node())) 
             then () 
             else compiler:compile-xpath( $node, $json )
       else compiler:compile-xpath( $node, $json ) 
    case element(section) return
      let $sNode := compiler:unpath( fn:string( $node/@name ) , $json, $pos, $xpath )
(: let $_ := compiler:log(("IN A SECTION ABOUT TO PROCESS", $sNode, "FOR", $node/@name)) :)
      return 
        if ( $sNode/@type = "boolean" and lower-case($sNode/text()) = 'true' ) 
        then compiler:compile-xpath( $node, $json, $pos, $xpath ) 
        else
          if ( $sNode/@type = "array" )
          then (
(:let $_ := compiler:log(("FOUND AN ARRAY")):)
            for $n at $p in $sNode/node()
(:let $_ := compiler:log(fn:concat($p,": ", xdmp:quote($n))):)
            return compiler:compile-xpath( $node, $json, $p, fn:concat( $xpath, '/', fn:node-name($sNode), '/_' ) ) )
          else if($sNode/@type = "object") then 
(:let $_ := compiler:log(("POSSIBLY AN OJBECT")) return:)
          compiler:compile-xpath( $node, $json, $pos, fn:concat( $xpath,'/', fn:node-name( $sNode ) ) ) else ()
    case text() return $node
    default return compiler:compile-xpath( $node, $json ) }; 

declare function compiler:eval( $node-name, $json, $pos ) { 
  compiler:eval($node-name, $json, $pos, '', fn:true() ) };
      
declare function compiler:eval( $node-name, $json, $pos, $xpath ) { 
  compiler:eval($node-name, $json, $pos, $xpath, fn:true() ) };

declare function compiler:eval( $node-name, $json, $pos, $xpath, $etag ) { 
  compiler:eval( $node-name, $json, $pos, $xpath, $etag, '' )
};

declare function compiler:eval( $node-name, $json, $pos, $xpath, $etag, $desc ) { 
(:let $_ := compiler:log(("****** COMPILER EVAL ETAG", $etag )):)
  let $unpath :=  compiler:unpath( $node-name, $json, $pos, $xpath, $desc )
  return try {
    let $value := serialize(xquery:eval( $unpath ))
    return if ($etag) 
    then fn:concat('{{b64:', xs:string(convert:string-to-base64($value)), '}}') (: recursive mustache ftw :)
    else $value
  } catch * { $unpath } };

declare function compiler:unpath( $node-name, $json, $pos, $xpath ) { 
  compiler:unpath( $node-name, $json, $pos, $xpath, '' )
};

declare function compiler:unpath( $node-name, $json, $pos, $xpath, $desc ) { 
  let $xp := fn:concat( 'declare variable $json external; ($json/json', $xpath, ')[', $pos, ']/',
    if ($desc='desc') then '/' else '', replace($node-name, '_', '__') => replace('@', '_0040') => replace(':', '_003a') ),
      $res := xquery:eval( $xp, map{'json': $json} )
      (: , $_ := compiler:log(("@@@@@ COMPILER UNPATH ", $node-name, "DESC", $desc, "SEARCHING FOR", $xp, "IN", serialize($json), "FOUND", serialize($res))) :)    
  return $res };

declare function compiler:handle-escaping( $div ) {
  for $n in $div/node()
  return compiler:handle-base64($n) };

declare function compiler:handle-base64( $node ) {
  typeswitch($node)
    case element()         return element {fn:node-name($node)} {
      for $a in $node/@*
      return attribute {fn:node-name($a)} {compiler:resolve-mustache-base64($a)}, 
      compiler:handle-escaping( $node )}
    case text()            return compiler:resolve-mustache-base64( $node)
    default                return compiler:handle-escaping( $node ) };

declare function compiler:resolve-mustache-base64( $text ) {
(: let $_ := compiler:log(("BASE64ing", $text)) return :)
 fn:string-join( for $token in fn:tokenize($text, " ")
  return 
    if ( fn:matches( $token, '\{\{b64:(.+?)\}\}' ) )
    then 
      let $as := fn:analyze-string($token, '\{\{b64:(.+?)\}\}')
      let $b64    := xs:base64Binary($as//*:group[@nr=1])
      let $before := $as/*:match[1]/preceding::*:non-match[1]/fn:string()
      let $after  := $as/*:match[fn:last()]/following::*:non-match[1]/fn:string()
      return fn:string-join( ($before, for $decoded in convert:binary-to-string( $b64 )
      let $executed := 
        if ( fn:matches( $decoded, "(&lt;|&gt;|&amp;|&quot;|&apos;)" ) )
        then fn:string($decoded)
        else fn:string(try { xquery:eval( $decoded ) } catch * { $decoded })
      return $executed, $after), '' )
    else if ( fn:matches( $token, '\{\{b64:\}\}' ) )
    then ""
    else $token, " ") };
    
declare %private function compiler:log($toLog as item()+) {
  admin:write-log(string-join($toLog, ' '), 'INFO')
};