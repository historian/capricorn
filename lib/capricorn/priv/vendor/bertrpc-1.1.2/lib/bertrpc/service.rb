module BERTRPC
  class Service
    attr_accessor :host, :port, :timeout

    def initialize(host, port, timeout = nil)
      @host = host
      @port = port
      @timeout = timeout
    end

    def call(options = nil)
      verify_options(options)
      Request.new(self, :call, options)
    end

    def cast(options = nil)
      verify_options(options)
      Request.new(self, :cast, options)
    end

    # private

    def verify_options(options)
      (options||{}).each do |key, value|
        case key
        when :stream
          unless (String === value and File.file?(value))
            raise InvalidOption.new("Valid :stream args are IO or path")
          end
        when :case
          unless value[0] == :validation && value[1].is_a?(String)
            raise InvalidOption.new("Valid :cache args are [:validation, String]")
          end
        else
          raise InvalidOption.new("Valid options are :cache and :stream")
        end
      end
    end
  end
end
