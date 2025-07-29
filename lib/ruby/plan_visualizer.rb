module Lantae
  class PlanVisualizer
    def self.generate_ascii_graph(plan)
      output = []
      
      # Header with plan info
      draw_header(output, plan)
      
      # Horizontal phase flow
      draw_horizontal_phases(output, plan['phases'])
      
      # Resource requirements and risks at bottom
      if plan['resource_requirements']
        draw_resources(output, plan['resource_requirements'])
      end
      
      if plan['risks'] && plan['risks'].any?
        draw_risks(output, plan['risks'])
      end
      
      output.join("\n")
    end
    
    private
    
    def self.draw_header(output, plan)
      title = plan['objective'] || 'Execution Plan'
      title_width = [title.length + 4, 80].max
      
      output << "┌─" + "─" * (title_width - 2) + "┐"
      output << "│ 📋 #{title.ljust(title_width - 4)} │"
      
      if plan['estimated_duration']
        duration_line = "⏱️  Duration: #{plan['estimated_duration']}"
        output << "│ #{duration_line.ljust(title_width - 4)} │"
      end
      
      if plan['success_criteria'] && plan['success_criteria'].any?
        output << "│ 🎯 Success: #{plan['success_criteria'].first.ljust(title_width - 15)} │"
      end
      
      output << "└─" + "─" * (title_width - 2) + "┘"
      output << ""
    end
    
    def self.draw_horizontal_phases(output, phases)
      return if phases.empty?
      
      # Calculate phase widths
      phase_width = 24
      total_width = phases.size * phase_width + (phases.size - 1) * 6 # 6 for arrows
      
      # Draw phase headers
      header_line = ""
      phases.each_with_index do |phase, idx|
        phase_name = truncate_text(phase['name'] || "Phase #{idx + 1}", phase_width - 4)
        header_line += "┌─#{phase_name.center(phase_width - 2)}─┐"
        header_line += "      " if idx < phases.size - 1 # spacing for arrow
      end
      output << header_line
      
      # Find max tasks in any phase for consistent height
      max_tasks = phases.map { |p| (p['tasks'] || []).size }.max || 0
      task_lines = [max_tasks, 5].max # minimum 5 lines for readability
      
      # Draw task content
      (0...task_lines).each do |line_idx|
        line = ""
        phases.each_with_index do |phase, phase_idx|
          tasks = phase['tasks'] || []
          
          if line_idx < tasks.size
            task = tasks[line_idx]
            task_name = truncate_text(task['name'] || '', phase_width - 4)
            
            # Color code by task type
            if task['parallel']
              prefix = "🔄"
            else
              prefix = "📋"
            end
            
            task_line = "#{prefix} #{task_name}"
            line += "│ #{task_line.ljust(phase_width - 2)} │"
          else
            line += "│" + " " * (phase_width - 2) + "│"
          end
          
          # Add arrow between phases
          if phase_idx < phases.size - 1
            if line_idx == task_lines / 2 # middle line gets the arrow
              line += "  ──→ "
            else
              line += "      "
            end
          end
        end
        output << line
      end
      
      # Bottom border
      bottom_line = ""
      phases.each_with_index do |phase, idx|
        bottom_line += "└─" + "─" * (phase_width - 2) + "┘"
        bottom_line += "      " if idx < phases.size - 1
      end
      output << bottom_line
      
      # Add duration info below each phase
      duration_line = ""
      phases.each_with_index do |phase, idx|
        total_duration = calculate_phase_duration(phase)
        duration_text = "⏱️  #{total_duration}".center(phase_width)
        duration_line += duration_text
        duration_line += "      " if idx < phases.size - 1
      end
      output << duration_line if duration_line.strip.length > 0
      output << ""
    end
    
    def self.calculate_phase_duration(phase)
      tasks = phase['tasks'] || []
      if tasks.any? && tasks.first['estimated_duration']
        # Sum up task durations (simplified)
        total_hours = tasks.map { |t| extract_hours(t['estimated_duration']) }.sum
        "#{total_hours}h"
      else
        phase['estimated_duration'] || '?'
      end
    end
    
    def self.extract_hours(duration_str)
      # Simple extraction of hours from strings like "4 hours", "2h", etc.
      return 0 unless duration_str
      
      if duration_str.match(/(\d+)\s*h/)
        $1.to_i
      elsif duration_str.match(/(\d+)\s*hour/)
        $1.to_i
      else
        1 # default
      end
    end
    
    def self.truncate_text(text, max_length)
      return text if text.length <= max_length
      text[0..max_length-4] + "..."
    end
    
    
    def self.draw_resources(output, resources)
      output << "🛠️  RESOURCES:"
      
      resource_items = []
      if resources['tools'] && resources['tools'].any?
        resource_items << "🔧 #{resources['tools'][0..2].join(', ')}"
      end
      
      if resources['models'] && resources['models']['preferred']
        resource_items << "🧠 #{resources['models']['preferred']}"
      end
      
      if resources['external_apis'] && resources['external_apis'].any?
        resource_items << "🌐 #{resources['external_apis'][0..1].join(', ')}"
      end
      
      output << resource_items.join('  •  ') if resource_items.any?
      output << ""
    end
    
    def self.draw_risks(output, risks)
      return if risks.empty?
      
      output << "⚠️  RISKS:"
      
      risk_summary = risks.take(3).map do |risk|
        color = case risk['impact']
        when 'high' then '🔴'
        when 'medium' then '🟡'
        else '🟢'
        end
        "#{color} #{risk['description'][0..30]}..."
      end
      
      output << risk_summary.join('  •  ')
      output << ""
    end
  end
end