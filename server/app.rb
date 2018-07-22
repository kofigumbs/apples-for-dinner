require "json"
require "logger"
require "net/http"
require "sinatra"

LOG = Logger.new STDOUT
API_KEY = ENV.fetch "AIRTABLE_API_KEY"
TABLE_URL = URI "https://api.airtable.com/v0/appOOHY2yfP6zFXzf/Webhook"

def on_signup
  LOG.info request.POST
  yield request.POST if request.POST["txn_type"] == "subscr_signup"
end

post "/webhook" do
  on_signup do |payload|
    custom = JSON.parse payload["custom"]
    record = {
      "fields" => {
        "Subscriber ID" => payload["subscr_id"],
        "Room" => custom.first,
        "Art" => custom.last,
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
end
