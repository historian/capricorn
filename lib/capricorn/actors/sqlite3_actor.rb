
module Capricorn
  module Actors # :nodoc:
    class Sqlite3Actor < Capricorn::Actor
      
      after_install_satellite :write_config_file
      
      # write the +database.yml+ config file.
      def write_config_file
        system.as_user(system.web_user, system.web_group) do
          
          config = %{# SQLite version 3.x
#   gem install sqlite3-ruby (not necessary on OS X Leopard)
development:
  adapter: sqlite3
  database: db/system/development.sqlite3
  pool: 5
  timeout: 5000

# Warning: The database defined as "test" will be erased and
# re-generated from your development database when you run "rake".
# Do not set this db to the same as development or production.
test:
  adapter: sqlite3
  database: db/system/test.sqlite3
  pool: 5
  timeout: 5000

production:
  adapter: sqlite3
  database: db/system/production.sqlite3
  pool: 5
  timeout: 5000
}
          
          db_file = File.join(system.satellite_root, 'config', 'database.yml')
          File.open(db_file, 'w+') { |f| f.write config }
          
        end
      end
      
    end
  end
end
