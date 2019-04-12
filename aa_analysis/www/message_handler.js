Shiny.addCustomMessageHandler('get_from_clipboard',
    function(message) {
      navigator.clipboard.readText().then(text => {
        Shiny.onInputChange('jsEvent0', text);
      }).catch(err => {
        console.error('Failed to read clipboard contents: ', err);
      });
    }
);
