autoload :Logger, 'logger'

module Shuttle
  module ExceptionHandler
    
    def self.setup(out=STDOUT, err=STDERR)
      if String === out
        @out = Logger.new(out, 'daily')
      else
        @out = Logger.new(out)
      end
      if String === err
        @err = Logger.new(err, 'daily')
      else
        @err = Logger.new(err)
      end
      @out.level = Logger::DEBUG
      @err.level = Logger::DEBUG
    end
    
    def self.err
      @err
    end
    
    def self.out
      @out
    end
    
    def logger
      Shuttle::ExceptionHandler
    end
    
    def log(*args, &block)
      logger.out.info(*args, &block)
    end
    
    def report
      yield
    rescue Exception => e
      if StandardError === e
        logger.err.error(e)
      else
        logger.err.fatal(e)
      end
      raise e
    end
    
  end
end

def FileUtils.fu_output_message(msg)
  Shuttle::ExceptionHandler.out.info(msg)
end