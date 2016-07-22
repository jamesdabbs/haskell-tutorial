require 'faker'
require 'pry'
require 'sequel'

require 'active_support/all' # for `days.ago`

db_path = File.expand_path "../twips.sqlite3", __FILE__
DB = Sequel.connect("sqlite://#{db_path}")
Users, Twip = DB[:user], DB[:twip]

[Twip, Users].each &:delete

users = 15.times.map do
  Users.insert ident: Faker::Name.name, password: Faker::Internet.password
end

50.times do
  Twip.insert(
    author_id:    users.sample,
    title:        Faker::Company.catch_phrase,
    body:         Faker::Lorem.paragraphs(rand 2..5).join("\n"),
    published_at: rand(1..7).days.ago
  )
end
