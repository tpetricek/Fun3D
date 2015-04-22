// Exposed so that it can be called by the page
var evaluateScript;
var funEditor;

function switchRotation(el) {
  if (el.innerHTML == "Stop rotation") {
    el.innerHTML = "Start rotation";
    document.fun3dRotation = 0.1;
  } else {
    el.innerHTML = "Stop rotation";
    document.fun3dRotation = 0.0;
  }
}
function loadDemo(name) {
  if (name == "") return;
  $.ajax("/content/demos/" + name + ".txt").done(function (r) {
    funEditor.setValue(r);
  });
}

$(function () {
  // Setup the CodeMirror editor with fsharp mode
  var editor = CodeMirror(document.getElementById('editor'),
  {
    value: "#load \"Fun3D.fsx\"\nopen Fun3D\n\nlet main() = \n  Fun.cube",
    mode: 'fsharp',
    lineNumbers: true
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
    $.ajax({
      url: "/run", data: editor.getValue(), contentType: "text/fsharp",
      type: "POST", dataType: "text"
    }).done(function (data) {
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
