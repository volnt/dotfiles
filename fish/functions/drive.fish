function drive
        if contains -- pull $argv
                rclone sync gdrive:DriveSyncFiles ~/gdrive
        else if contains -- push $argv
                rclone sync ~/gdrive gdrive:DriveSyncFiles
        else
                echo 'Usage : drive (pull|push)'
        end
end
