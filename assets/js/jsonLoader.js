function handleFileSelect(evt) {
  var file = evt.target.files[0];
  var reader = new FileReader();
  reader.readAsText(file);

  reader.onload = (function() {
    return function(e) {
      page.ports.fileReader.send({fileText: e.target.result});
    };
  })(file);
}

document.getElementById('fileLoader').addEventListener('change', handleFileSelect, false);
