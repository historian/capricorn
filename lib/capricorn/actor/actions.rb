
module Capricorn
  class Actor
    module Actions
      
      def self.included(base)
        base.extend Capricorn::Actor::Actions::ClassMethods
      end
      
      # run the callbacks registerd to <tt>action</tt>. the will run the <tt>before</tt>, <tt>on</tt> and <tt>after</tt> fases.
      def run_callbacks!(action)
        action = action.to_sym
        run_callbacks_in_fase! action, :before
        run_callbacks_in_fase! action, :on
        run_callbacks_in_fase! action, :after
      end
      
      # run the callbacks registerd to <tt>action</tt> in the +fase+
      def run_callbacks_in_fase!(action, fase)
        action = action.to_sym
        fase   = fase.to_sym
        self.class.methods_for(action, fase).each { |meth| self.send(meth) }
      end
      
      module ClassMethods
        
        # register a callback to be run <tt>before</tt> the <tt>action</tt>.
        def before(action, meth)
          methods_for(action, :before).push(meth.to_sym)
        end
        
        # register a callback to be run <tt>on</tt> the <tt>action</tt>.
        def on(action, meth)
          methods_for(action, :on).push(meth.to_sym)
        end
        
        # register a callback to be run <tt>after</tt> the <tt>action</tt>.
        def after(action, meth)
          methods_for(action, :after).push(meth.to_sym)
        end
        
        # get the registered callbacks for the +action+ and +face+.
        def methods_for(action, fase)
          action = action.to_sym
          fase   = fase.to_sym
          
          @actions ||= {}
          @actions[action] ||= {}
          @actions[action][fase] ||= []
          @actions[action][fase]
        end
        
        # define one or more actions.
        def action(*names)
          names.each do |name|
            module_eval %{
              def self.before_#{name}(meth)
                before(:#{name}, meth)
              end
              def self.on_#{name}(meth)
                on(:#{name}, meth)
              end
              def self.after_#{name}(meth)
                after(:#{name}, meth)
              end
              def run_#{name}_callbacks!
                run_callbacks!(:#{name})
              end
            }
          end
        end
        
      end
    end
  end
end