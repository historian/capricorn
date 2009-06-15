class EngineGenerator < RubiGen::Base
  attr_reader :engine_name

  def initialize(runtime_args, runtime_options = {})
    super
    @destination_root = File.expand_path('.')
    @engine_name = args.shift.underscore
  end
  
  def manifest
    record do |m|
      
      m.directory "app/controllers"
      m.directory "app/models"
      m.directory "app/views"
      m.directory "app/helpers"
      m.directory "config"
      m.directory "db/migrate"
      m.directory "lib"
      m.directory "public/images"
      m.directory "public/javascripts"
      m.directory "public/stylesheets"
      m.directory "rails"
      m.directory "tasks"
      
      m.template('config/routes.rb',                  "config/routes.rb")
      m.template('config/initializers/rails_init.rb', "config/initializers/rails_init.rb")
      m.template('Gmfile',                            "Gmfile")
      m.template('rails/init.rb',                     "rails/init.rb")
      m.template('init.rb',                           "init.rb")
      m.template('tasks/engine_tasks.rake',           "tasks/#{engine_name}_tasks.rake")
      m.template('README.rdoc',                       "README.rdoc")
      m.template('MIT-LICENSE.txt',                   "MIT-LICENSE.txt")
      m.template('lib/engine.rb',                     "lib/#{engine_name}.rb")
      m.template('gitignore',                         ".gitignore")
      
    end
  end
  
end