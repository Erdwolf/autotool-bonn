database ./autotool/db

db /config : map(string, string)

type msg_to_client = 
    { server_answer : string } 
  / { server_error : string }
       

update_client(_ : void, msg : msg_to_client) =
  do match msg with
  | { server_answer = ans } ->
    exec([#test <- "Answer: {ans}"])
  | { server_error = err } ->
    exec([#test <- "Error: {err}"])
  {unchanged}

submit_config(client_session, id) =
  do exec([#main +<- <div id="test">test</div>])
  text = jQuery.getVal(#{id})
  do send(server_session, {configuration = (client_session, text)})
  void


check_config() =
  text = jQuery.getVal(#config)
  do match ?/config[text] with
   {none} -> exec([#cache <- "no cache"])
   {~some} -> exec([#cache <- "cache: {some}"])
  void

css = css
body {
  color: #4D4D4D;
}

.button {
  font-weight: bolder;
  cursor: pointer;
}

main_page() =
  start() =
    client_session = session(void, update_client)
    rest =
      <>
        <textarea id="config"></textarea>
        <select id="select" onchange={ _ -> 
	    jlog(jQuery.getVal(#select))
	}>{ List.map((n -> <option value={n}>{n*n}</option>),[1,2,3,42]) }</select>
        <div class="button" onclick={ _ -> check_config()}>check cache</div>
        <div id="cache"></div>
        <div class="button" onclick={ _ -> submit_config(client_session, "config")}>Submit config</div>
      </>
    do exec([#main +<- rest])
    void
  page =
    <div id="main" onload={ _ -> start()}>
      <h1>Hello</h1>
    </div>
  html("Autotool",page)

urls = parser
  | .* -> main_page()

server = simple_server(urls)


type msg_to_server = 
    { configuration : (channel(msg_to_client),string) } 

server_callback(_ : void, msg : msg_to_server) =
  do match msg with
  | { configuration = (ch, cfg) } ->
    loc = 
      { Http_client.default_location with
        host = "nfa.imn.htwk-leipzig.de";
        port =  9876;
        path = "/VerifyTaskConfig?task=foo&config={cfg}" }
    callback(answer : string) = 
      do /config[cfg] <- answer
      send(ch, { server_answer = answer})
    Http_client.get(loc, callback)
  {unchanged}

server_session = session(void, server_callback)
