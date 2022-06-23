disableInputs = function(data){


  /* disable inputs. standard inputs use 'disabled' attribute */
  let inputs = $("#" + data.id + " .shiny-input-container input");
                                               
   for (var i = 0; i < inputs.length; i++) {
     inputs[i].setAttribute("disabled","");
   }

  /* disable selectizeinputs. these are special objects with their own method */
  /* see shinyjs code: shinyjs-default-funcs.js, _enableDisable*/
  let selects = $("#" + data.id + " .shiny-input-container .selectized");
                     
  for (var i = 0; i < selects.length; i++) {
    $(selects[i]).selectize()[0].selectize.disable();
  }
    
}
                 
Shiny.addCustomMessageHandler("disableInputs", disableInputs);
