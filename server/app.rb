require "json"
require "logger"
require "net/http"
require "sinatra"

LOG = Logger.new STDOUT
API_KEY = ENV.fetch "AIRTABLE_API_KEY"
TABLE_URL = URI "https://api.airtable.com/v0/appOOHY2yfP6zFXzf/Webhook"

post "/webhook" do
  record = {
    "fields" => {
      "Subscriber ID" => request.POST["subscr_id"],
      "Transaction" => request.POST["txn_type"],
      "Custom" => request.POST["custom"],
      "Test" => request.POST["test_ipn"].to_i == 1,
    }
  }
  Net::HTTP.start(TABLE_URL.hostname, TABLE_URL.port, use_ssl: true) do |http|
    response = http.post(
      TABLE_URL.path,
      record.to_json,
      "Content-Type" => "application/json",
      "Authorization" => "Bearer #{API_KEY}")
    LOG.info response.read_body
  end
end
