module Capr::Do

  class Action

    include EM::Deferrable

    class << self

      def define_callback(name, fire_method=nil, bind_method=nil)
        bind_method ||= "on_#{name}"
        fire_method ||= "fire_#{name}"
        class_eval %{
          def #{fire_method}(*args)
            return true unless @#{name}
            @#{name}.each do |clb|
              clb.call(*args)
            end
            return true
          end

          def #{bind_method}(proc=nil, &block)
            proc = proc || block
            if proc
              @#{name} ||= []
              @#{name}.push(proc)
            end
          end
        }
      end

      def initial_method
        @initial_method ||= (superclass.initial_method rescue :start)
      end

      attr_writer :initial_method

    end
    
    def call(*args, &block)
      __send__(self.class.initial_method, *args, &block)
      self
    end
    
  end

  def self.Action(initial_method)
    klass = Class.new(Capr::Do::Action)
    klass.initial_method = initial_method
    klass
  end

end