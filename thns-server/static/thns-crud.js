
var getConvAll = function()
{
  return axios({ url: '/conv/all'
    , method: 'get'
    });
}



var postConv = function(body)
{
  return axios({ url: '/conv'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}



var putConvById = function(id, body)
{
  return axios({ url: '/conv/' + encodeURIComponent(id) + ''
    , method: 'put'
    , data: body
    , responseType: 'json'
    });
}



var deleteConvById = function(id)
{
  return axios({ url: '/conv/' + encodeURIComponent(id) + ''
    , method: 'delete'
    });
}
