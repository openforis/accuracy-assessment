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
      const key = 'projectId';
      const match = ceoUrl.match('[?&]' + key + '=([^&#]+)');
      const id = match ? match[1] : null;
      if (!isNaN(id)) {
        const xmlHttp = new XMLHttpRequest();
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
