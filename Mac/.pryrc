# Pager のバグ対応
# ver 0.10.x では less しか対応していない模様
Pry.config.pager = false

# プロンプトの設定
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

# # hirb の設定
# begin
#   require 'hirb'
# rescue LoadError
#   # Missing goodies, bummer
# end

# if defined? Hirb
#   # Slightly dirty hack to fully support in-session Hirb.disable/enable toggling
#   Hirb::View.instance_eval do
#     def enable_output_method
#       @output_method = true
#       @old_print = Pry.config.print
#       Pry.config.print = proc do |output, value|
#         Hirb::View.view_or_page_output(value) || @old_print.call(output, value)
#       end
#     end

#     def disable_output_method
#       Pry.config.print = @old_print
#       @output_method = nil
#     end
#   end

#   Hirb.enable
# end
