Shiny.addCustomMessageHandler('create_ceo_project',
    function(message) {
        const { classes, title, plotSize, csv } = message;
        const rows = csv.split('\n');
        rows.shift(); // remove first element
        if (rows[rows.length - 1] === '') rows.pop(); // remove last element if empty
        const plots = rows.map((row) => {
            const values = row.split(',');
            const [x, y] = values;
            return {
                lon: x.replace(/"/g, ''),
                lat: y.replace(/"/g, '')
            };
        });
        const json = {
            classes,
            title,
            plotSize: parseInt(plotSize),
            plots
        };
        const xmlHttp = new XMLHttpRequest();
        xmlHttp.open('POST', '/api/ceo-gateway/create-project', false); // false for synchronous request
        xmlHttp.setRequestHeader('Content-Type', 'application/json');
        xmlHttp.send(JSON.stringify(json));
        console.log(json);
        const {status, responseText} = xmlHttp;
        let res = JSON.parse(responseText); // projectId, ceoCollectionUrl, errorMessage
        res = {
          ...res,
          status,
          title
        };
        Shiny.onInputChange('jsEvent', res);
    }
);
