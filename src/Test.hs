{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test where

import Language.Haskell.TH
import Text.CSharp

servantClientCode :: String
servantClientCode = [csharp|
using Newtonsoft.Json;
using System.Collection.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

#region type aliases
$forall (name, type) <- types
  using #{name} = #{type}
#endregion

namespace ServantClientBook
{
    class ServantClient : HttpClient
    {
        public ServantClient()
        {
            this.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }
    }

    public class API
    {
        #region fields
        private string server;
        #endregion

        #region properties
        #endregion

        #region Constructor
        public API(string _server)
        {
            this.server = _server;
        }
        #endregion

        #region APIs
        #:forall ep <- endpoints
          public async Task<#{retType ep}> #{methodName ep}Async(#{paramDecl ep})
          {
              var client = new ServantClient();
              var queryparams = new List<string> {
                  $:forall qp <- queryparams ep
                    _#{qp}.HasValue ? $"_#{qp}={_#{qp}.Value}" : null,
              }.Where(e => !string.IsNullOrEmpty(e));
              var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
#if DEBUG
                var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
#else
                var jsonObj = JsonConvert.SerializeObject(_obj);
#endif
              #:if requestBodyExists ep
                var res = await client.#{methodType ep}Async($"{server}#{uri ep}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
              #:else
                var res = await client.#{methodType ep}Async($"{server}#{uri ep}{qp}");
              Debug.WriteLine($">>> {res.RequestMessage}");
              #:if requestBodyExists ep
                Debug.WriteLine($"-----");
                Debug.WriteLine(jsonObj);
                Debug.WriteLine($"-----");
              Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
              var content = await res.Content.ReadAsStringAsync();
              Debug.WriteLine($"<<< {content}");
              return JsonConvert.DeserializeObject<${retType ep}>(content);
         }
          public #{retType ep} #{methodName ep}(#{paramDecl ep})
          {
              Task<#{retType ep}> t = #{methodName ep}Async(#{paramArg ep});
              return t.GetAwaiter().GetResult();
          }
        #endregion
    }
}
       |]

main = do
  let types = [ ("AddressId", "System.Int64")
              , ("AuthorId", "System.Int64")
              , ("PublisherId", "System.Int64")
              , ("BookId", "System.Int64")
              , ("ISBN", "System.String")
              , ("Postcode", "System.String")
              , ("Tel", "System.String")
              , ("Fax", "System.String")
              , ("Emailaddress", "System.String")
              ]
  putStr servantClientCode
