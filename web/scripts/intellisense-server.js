// Exposed so that it can be called by the page
var evaluateScript;
var funEditor;

// Switch rotation, bsaed on the text in the button..
function switchRotation(el) {
  if (el.innerHTML == "Stop rotation") {
    el.innerHTML = "Start rotation";
    document.fun3dRotation = 0.00;
  } else {
    el.innerHTML = "Stop rotation";
    document.fun3dRotation = 0.01;
  }
}

// Load demo from the specified text file in content/demos
function loadDemo(name) {
  if (name == "") return;
  $.ajax("/content/demos/" + name + ".txt").done(function (r) {
    funEditor.setValue(r);
  });
}

// Validate title, author and info values. If everything is good,
// send /share request to the server, wait & display confirmation
function saveAndShare()
{
  // Validate that all values have been set
  var title = document.getElementById("submit-title").value;
  var author = document.getElementById("submit-author").value;
  var info = document.getElementById("submit-info").value;
  var pub = document.getElementById("submit-public").checked;
  if (title == "" || author == "" || info == "")
  {
    $("#submit-error").css('visibility', 'visible');
    return;
  }

  // Send data using AJAX to the server
  var data = {
    "title": title, "author": author,
    "info": info, "public": pub, "code": funEditor.getValue(),
  };
  $.ajax({
    url: "/share", data: JSON.stringify(data),
    contentType: "application/json", type: "POST", dataType: "JSON"
  }).done(function (res) {
    // Display the confirmation window with links
    document.getElementById("result-url").value = res.url;
    document.getElementById("result-link").href = res.url;
    $("#modal-share").css("display", "none");
    $("#modal-done").css("display", "block");
  });
}

// Close the share dialog and reset it to initial state
function closeShareDialog()
{
  window.setTimeout(function () {
    document.getElementById("submit-title").value = "";
    document.getElementById("submit-author").value = "";
    document.getElementById("submit-info").value = "";
    document.getElementById("submit-public").checked = true;
    $("#modal-share").css("display", "block");
    $("#modal-done").css("display", "none");
  }, 500);
}

$(function () {
  // Setup the CodeMirror editor with fsharp mode
  var editor = CodeMirror(document.getElementById('editor'), {
    value: "Fun.cube", mode: 'fsharp', lineNumbers: true
  });
  editor.focus();
  funEditor = editor;

  // Helper to send request to our server
  function request(operation, line, col) {
    var url = "/" + operation;
    if (line != null && col != null) url += "?line=" + (line + 1) + "&col=" + col;
    return $.ajax({
      url: url, data: editor.getValue(),
      contentType: "text/fsharp", type: "POST", dataType: "JSON"
    });
  }

  // Translate code to JS and then evaluate the script
  evaluateScript = function () {

    var counter = 0;
    var finished = false;
    function spin() {      
      if (finished) {
        document.getElementById("spinner").style.display = "none";
        return;
      } else {
        document.getElementById("spinner").style.display = "block";
        var offset = counter * -21;
        document.getElementById("spinner").style.backgroundPosition = "0px " + offset + "px";
        counter++; if (counter >= 19) counter = 0;
        setTimeout(spin, 100);
      }
    };
    setTimeout(spin,500);

    $.ajax({
      url: "/run", data: editor.getValue(), contentType: "text/fsharp",
      type: "POST", dataType: "text"
    }).done(function (data) {
      finished = true;
      eval("(function(){ " + data + "})()");
    });
  };

  // Request type-checking and parsing errors from the server
  editor.compiler = new Compiler(editor, function () {
    request("check").done(function (data) {
      editor.compiler.updateMarkers(data.errors);
    });
  });

  // Request declarations & method overloads from the server
  editor.intellisense = new Intellisense(editor,
    function (position) {
      request("declarations", position.lineIndex, position.columnIndex).done(function (data) {
        editor.intellisense.setDeclarations(data.declarations);
      });
    },
    function (position) {
      request("methods", position.lineIndex, position.columnIndex).done(function (data) {
        editor.intellisense.setMethods(data.methods);
      });
    }
  );

  document.getElementById("editor").height = 500;
  editor.setSize("100%", 500);
});
