class Capricorn::CLI::ApplicationsDomains < Capricorn::CLI

  include Capricorn::Helpers

  namespace "apps:domains"

  desc "list", "list all domains"
  def list
    application_info[4].sort! do |a, b|
      a <=> b
    end
    application_info[4].each do |domain|
      puts "- #{domain}"
    end
  end

  desc "add DOMAIN", "add a new domain"
  def add(domain)
    domain = domain.strip.sub(/^www\./, '')

    begin
      domain = PublicSuffixService.parse(domain).to_s
    rescue Exception
      halt "invalid domain: #{domain}"
    end

    machine, id = *application
    app         = application_info

    if app[4].include?(domain)
      halt "Domain is already configured"
    end

    app[4].push(domain)

    p client.call.applications.update(machine.to_sym, id, app[4], app[10])
  end

  desc "remove DOMAIN", "remove a domain"
  def remove(domain)
    domain = domain.strip.sub(/^www\./, '')

    begin
      domain = PublicSuffixService.parse(domain).to_s
    rescue Exception
      halt "invalid domain: #{domain}"
    end

    machine, id = *application
    app         = application_info

    unless app[4].include?(domain)
      halt "Domain is not configured"
    end

    app[4].delete(domain)

    p client.call.applications.update(machine.to_sym, id, app[4], app[10])
  end

end