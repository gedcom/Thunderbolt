makeJShandler = function()
{
  out = {
    tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    ')))
  }
}

updateInputEnabled = function (id, session, enabled)
{
  require(shiny)
  tfstring = if (enabled) "false" else "true"
  
  msg = list(code = paste0("$('#", id, "').prop('disabled', ", tfstring ,")"))
  session$sendCustomMessage(type = "jsCode", msg)
}

disableInput = function(id, session) updateInputEnabled(id, session, FALSE)
enableInput = function(id, session) updateInputEnabled(id, session, TRUE)