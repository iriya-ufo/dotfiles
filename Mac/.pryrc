# Prompt
Pry.config.prompt = [
  proc {|target_self, nest_level, pry|
    nested = (nest_level.zero?) ? '' : ":#{nest_level}"
    "[#{pry.input_array.size}] pry #{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}(#{Pry.view_clip(target_self)})#{nested}> "
  },
  proc {|target_self, nest_level, pry|
    nested = (nest_level.zero?) ?  '' : ":#{nest_level}"
    "[#{pry.input_array.size}] pry #{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}(#{Pry.view_clip(target_self)})#{nested}* "
  }
]

# refs: https://github.com/pry/pry/wiki/FAQ#wiki-hirb
if defined? Hirb
 Hirb::View.instance_eval do
   def enable_output_method
     @output_method = true
     @old_print = Pry.config.print
     Pry.config.print = proc do |output, value|
       Hirb::View.view_or_page_output(value) || @old_print.call(output, value)
     end
   end

   def disable_output_method
     Pry.config.print = @old_print
     @output_method = nil
   end
 end

 Hirb.enable
end

# refs: https://github.com/deivid-rodriguez/pry-byebug#matching-byebug-behaviour
if defined?(PryByebug)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
end
