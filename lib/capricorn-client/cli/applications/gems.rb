class Capricorn::CLI::ApplicationsGems < Capricorn::CLI

  include Capricorn::Helpers

  namespace "apps:gems"

  desc "list", "list all gems"
  def list
    (application_info[9] || []).sort! do |a,b|
      a[1] <=> b[1]
    end

    (application_info[10] || []).sort! do |a,b|
      a <=> b
    end

    puts "Configured gems:"
    (application_info[10] || []).each do |name|
      puts "- #{name}"
    end
    puts

    puts "Used gems:"
    (application_info[9] || []).each do |gem|
      puts "- #{gem[1]} (#{gem[2].flatten.join('.')})"
    end
  end

  desc "add GEM_NAME", "add a new gem"
  def add(gem_name)
    gem_name = gem_name.strip

    machine, id = *application
    app         = application_info

    app[10] ||= []

    if app[10].include?(gem_name)
      halt "Gem is already configured"
    end

    app[10].push(gem_name)

    p client.call.applications.update(machine.to_sym, id, app[4], app[10])
  end

  desc "remove GEM_NAME", "remove a gem"
  def remove(gem_name)
    gem_name = gem_name.strip

    machine, id = *application
    app         = application_info

    app[10] ||= []

    unless app[10].include?(gem_name)
      halt "Gem is not configured"
    end

    app[10].delete(gem_name)

    p client.call.applications.update(machine.to_sym, id, app[4], app[10])
  end

end