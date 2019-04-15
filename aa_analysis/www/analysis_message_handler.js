Shiny.addCustomMessageHandler('get_from_clipboard',
    function(message) {
      navigator.clipboard.readText().then(text => {
        Shiny.onInputChange('jsEvent0', text);
      }).catch(err => {
        console.error('Failed to read clipboard contents: ', err);
      });
    }
);

Shiny.addCustomMessageHandler('import_ceo_project',
    function(message) {
      const { ceoUrl } = message;
      const id = ceoUrl.split('/').slice(-1)[0];
      if (!isNaN(id)) {
        const xmlHttp = new XMLHttpRequest();
        alert(`/api/ceo-gateway/get-collected-data/${id}`);
        xmlHttp.open('GET', `/api/ceo-gateway/get-collected-data/${id}`, false);
        xmlHttp.send();
        const {status, responseText} = xmlHttp;
        let res = {
          id,
          status: xmlHttp.status,
          responseText: xmlHttp.responseText
        };
        Shiny.onInputChange('jsEvent1', res);
      }
    }
);
