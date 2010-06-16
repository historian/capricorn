class Capricorn::Util::Internal < Capricorn::Util
  namespace :internal

  require 'capricorn'
  require 'rubygems/format'

  desc "scaffolder", "scaffolder process for capricorn"
  def scaffolder
    Erlang do |cmd|
      case cmd.shift
      when :create
        recipe = cmd.shift
        app = convert_app(cmd.first)

        recipe_path = File.expand_path("../../lib/capricorn/recipes/#{recipe}.rb", __FILE__)
        ctx = Capricorn::SystemContext.run(recipe_path, :application => app)

        send t[true, t[ctx.www_user, ctx.www_group, ctx.root_path]]
      end
    end
  end

  desc "inspector", "inspector process for capricorn"
  def inspector
    Erlang do |path|

      unless File.extname(path) == ".gem" and File.file?(path)
        error :not_found
      end

      begin
        format = Gem::Format.from_file_by_path(path)
      rescue Exception => e
        error :gem_error, e.message
      end

      spec = format.spec
      unless spec
        error :gem_error, "Invalid gem"
      end

      dependencies = spec.runtime_dependencies.collect do |dep|
        version_requirements = dep.version_requirements.as_list.collect do |req|
          op, version = *req.split(/\s+/, 2)
          BERT::Tuple[op, version]
        end
        BERT::Tuple[dep.name, version_requirements]
      end

      send BERT::Tuple[spec.name, spec.version.to_s, dependencies]

    end
  end

private

  def convert_app(app)
    application = {}
    application[:id]          = app[1]
    application[:node]        = app[2]
    application[:name]        = app[3]
    application[:domain]      = app[4].shift
    application[:aliases]     = app[4]
    application[:environment] = app[5]
    application
  end

end