# Installation Guide for Muse: A High-Performance Music Synthesis System

## Prerequisites
Before you begin installing Muse on your system, please ensure that both a Common Lisp implementation and an audio library are installed. We recommend using the Steel Bank Common Lisp (SBCL) compiler due to its optimal performance characteristics. The following dependencies must also be present:

- **Quicklisp**: A popular package manager for Common Lisp.
- **PortAudio** or **OpenAL** libraries: These handle sound output in Muse's environment.

### Installing Prerequisites
1. **Steel Bank Common Lisp (SBCL)**:
   - On macOS:
     ```sh
     brew install sbcl
     ```
   - On Ubuntu/Debian:
     ```sh
     sudo apt-get install sbcl
     ```
   - On Windows:
     Download and install SBCL from the [official website](http://www.sbcl.org/platform-table.html).

2. **Quicklisp**:
   - Download and install Quicklisp:
     ```lisp
     (progn
       (let ((quicklisp-url "https://beta.quicklisp.org/quicklisp.lisp"))
         (with-open-stream (s (http-get quicklisp-url))
           (load s)))
       (quicklisp-quickstart:install)
       (ql:add-to-init-file))
     ```

3. **PortAudio** or **OpenAL**:
   - On macOS:
     ```sh
     brew install portaudio
     ```
   - On Ubuntu/Debian:
     ```sh
     sudo apt-get install libportaudio2 libopenal-dev
     ```
   - On Windows:
     Download and install PortAudio from the [official website](http://www.portaudio.com/download.html).

## Cloning the Repository
Once you have installed the prerequisites, you can clone the Muse repository from GitHub.

1. Open a terminal and run the following command:
   ```sh
   git clone https://github.com/Mignonne-Patterson/Muse.git
   ```

Navigate to the Muse directory:
sh

    cd Muse

Building Muse

After cloning the repository, you need to build Muse using SBCL and Quicklisp.

    Load the Muse system:
    lisp

(ql:quickload "muse")

Compile the system:
lisp

    (sb-ext:save-lisp-and-die "musesynth" :executable t :toplevel #'muse:start)

Running Muse

To run Muse, simply execute the compiled binary or load it in your Common Lisp environment.

    Execute the binary:
    sh

./musesynth

Alternatively, load Muse in SBCL:
lisp

    (load "musesynth")
    (muse:start)

Troubleshooting

If you encounter any issues during installation or usage, consider the following steps:

    Check Dependencies: Ensure all required dependencies are installed and properly configured.
    Consult Documentation: Refer to the Muse documentation and Common Lisp resources for additional guidance.
    Community Support: Reach out to the community for support via GitHub issues or discussion forums.

Updating Muse

To update Muse to the latest version, navigate to the Muse directory and pull the latest changes from the repository.

    Open a terminal and run the following command:
    sh

git pull origin main

Rebuild Muse:
lisp

    (ql:quickload "muse")
    (sb-ext:save-lisp-and-die "musesynth" :executable t :toplevel #'muse:start)

Uninstallation

If you need to uninstall Muse, you can remove the cloned repository and any compiled binaries.

    Delete the Muse directory:
    sh

rm -rf Muse

Remove the compiled binary:
sh

    rm musesynth

Conclusion

Muse provides a powerful and flexible environment for music synthesis. By following this installation guide, you should be able to set up and start using Muse effectively. For more information and advanced usage, refer to the comprehensive documentation provided within the Muse repository.

Happy synthesizing!
