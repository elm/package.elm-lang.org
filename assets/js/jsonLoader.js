function handleFileSelect(evt) {
  var file = evt.target.files[0];

  // for some reason type doesn't work on json files,
  // so this is the next best thing.
  if (!/.*\.json$/.test(file.name)) {
    return page.ports.fileReader.send(null);;
   }

  var reader = new FileReader();
  reader.readAsText(file);

  reader.onload = (function() {
    return function(e) {
      page.ports.fileReader.send({fileText: e.target.result});
    };
  })(file);
}

document.getElementById('fileLoader').addEventListener('change', handleFileSelect, false);
