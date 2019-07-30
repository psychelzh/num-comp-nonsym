% preparation works
configs = jsondecode(fileread('config.json'));
config_stim = readtable('config_stim.txt');
config_stim.stim = cellfun(...
    @(x) struct2table(jsondecode(x)), config_stim.stim, ...
    'UniformOutput', false);

PsychDefaultSetup(2)
window_ptr = PsychImaging('OpenWindow', 0, WhiteIndex(0));
n_stim = height(config_stim);
for i_stim = 1:n_stim
    this_config = config_stim(i_stim, :);
    this_stimuli = this_config.stim{:};
    n_dots = height(this_stimuli);
    rect_dots = nan(4, n_dots);
    color_dots = nan(3, n_dots);
    for i_dot = 1:n_dots
        this_dot = this_stimuli(i_dot, :);
        rect_this_dot_base = [0, 0, 2 * this_dot.size, 2 * this_dot.size];
        rect_dots(:, i_dot) = CenterRectOnPoint(rect_this_dot_base, this_dot.position_x, this_dot.position_y);
        switch this_dot.color{:}
            case 'r'
                color_dot = [1, 0, 0];
            case 'b'
                color_dot = [0, 0, 1];
        end
        color_dots(:, i_dot) = color_dot;
    end
    Screen('FillOval', window_ptr, color_dots, rect_dots);
    Screen(window_ptr, 'Flip');
    image_array = Screen('GetImage', window_ptr, [0, 0, configs.range_position(1), configs.range_position(2)]);
    imwrite(image_array, fullfile('images', this_config.pic_file{:}))
end
sca;
