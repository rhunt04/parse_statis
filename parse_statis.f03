MODULE utils
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp = kind(1.d0)

  ! Default DL_POLY STATIS file name to read from.
  ! ... and the format of it's major data blocks
  ! (see <dl_poly_dir>/source/statistics.F90)
  CHARACTER(*), PARAMETER :: statis_default = "STATIS"
  CHARACTER(*), PARAMETER :: statis_read_fmt = "(E14.6)"

  ! Default output file to write to.
  ! ... and the format we will write as.
  ! (NB same precision as DL_POLY read - don't want to manufacture more sig.
  ! digits - just make the printing nicer.)
  CHARACTER(*), PARAMETER :: output_default = "PARSED"
  CHARACTER(*), PARAMETER :: output_write_fmt = "(ES20.10)"

  ! See DL_POLY_4.0 manual - specifically the section on the "STATIS" file
  ! for info about this data.
  CHARACTER(*), DIMENSION(28), PARAMETER :: label_list = [ &
    &"engcns", "  temp", "engcfg", "engsrc", "engcpe",&
    &"engbnd", "engang", "engdih", "engtet", "enthal",&
    &"tmprot", "   vir", "virsrc", "vircpe", "virbnd",&
    &"virang", "vircon", "virtet", "volume", "tmpshl",&
    &"engshl", "virshl", " alpha", "  beta", " gamma",&
    &"virpmf", " press", " consv" ]
    ! ... and some unsupported variable tags I'm leaving out.
    !
    ! These could be added easily: their description is as follows:
    !
    ! `ntpatm` more entries for MSD of atom types.
    !   > leaving because requires parsing output or input  file.
    !
    ! then 9 entries for stress tensor.
    !   > if wanted, could grab, but do I want...?
    !   > again, knowing where this is requires parsing output or input file.
    !
    ! then 10 entries IF NPT/NST - containing cell vectors and stpipv (P . V)
    !   > volume will suffice for thermal expansion?
    !   > requires (separate) additional parsing, as above.
    !
    ! then 2 entries IF NPT or NP_nAT: MD cell height and normal surface.
    !   > lol wat?
    !   > requires (separate) additional parsing, as above.
    !
    ! then 2 entries IF NPT or N\gamma_nAT: surface tension x,y components on
    !   normal surface.
    !   > *cough* again lol wat?
    !   > requires (separate) additional parsing, as above.
    !

  INTEGER :: g_write_skip = 1
  INTEGER :: g_ignore_lines = 0
  LOGICAL :: g_avg_only = .false.

  PRIVATE

  PUBLIC dp, label_list
  PUBLIC g_ignore_lines, g_write_skip, g_avg_only

  PUBLIC statis_default, output_default
  PUBLIC statis_read_fmt, output_write_fmt

  PUBLIC handle_args
  PUBLIC get_labels_interactive
  PUBLIC transfer_header
  PUBLIC read_statis_values
  PUBLIC write_labels
  PUBLIC filter_values, handle_values, accumulate_stats, write_stats
  PUBLIC finish
  INTERFACE write_rows
    MODULE PROCEDURE write_rows_dp, write_rows_char
  END INTERFACE write_rows
  PUBLIC write_rows

  CONTAINS

  SUBROUTINE get_int(str, i, ierr, pos)
    ! Given a char, try to read an int.
    ! Optionally make sure it's > 0.
    IMPLICIT NONE
    CHARACTER(*), INTENT(in) :: str
    INTEGER, INTENT(out) :: i, ierr
    LOGICAL, OPTIONAL, INTENT(in) :: pos

    read(str, *, iostat = ierr) i

    if ( present(pos) ) then
      if ( pos ) then
        if ( i .le. 0 ) ierr = 1
      endif
    endif

  END SUBROUTINE get_int

  SUBROUTINE handle_args(statis_file, output_file, got_labels, labels)
    IMPLICIT NONE
    INTEGER :: i, num_args, iarg, ierr
    CHARACTER(80) :: arg
    CHARACTER(*), INTENT(out) :: statis_file, output_file
    LOGICAL, INTENT(OUT) :: got_labels
    INTEGER, ALLOCATABLE, INTENT(inout) :: labels(:)
    LOGICAL :: label_ok = .false.

    num_args = command_argument_count()

    statis_file = statis_default
    output_file = output_default

    if ( num_args > 0 ) then

      ! loop across options
      do iarg = 1, num_args, 2
        call get_command_argument(iarg, arg)
        select case ( trim(adjustl(arg)) )

          case("--labels", "-l")
            got_labels = .true.

            ! Get the labels...
            call get_command_argument(iarg + 1, arg)

            ! Check the labels...
            call delim_separated_extract(arg, labels, label_ok)
            if ( .not. label_ok ) call usage("bad label string")

          case("--statis_file", "-s")
            call get_command_argument(iarg + 1, arg)
            statis_file = arg

          case("--ignore", "-i")
            call get_command_argument(iarg + 1, arg)
            call get_int(arg, i, ierr, .true.)
            if ( ierr /= 0 ) call usage("bad number of ignore lines")
            g_ignore_lines = i

          case("--write-skip", "-w")
            call get_command_argument(iarg + 1, arg)
            call get_int(arg, i, ierr, .true.)
            if ( ierr /= 0 ) call usage("bad number of write-skip lines")
            g_write_skip = i

          case("--averages", "-a")
            g_avg_only = .true.
            write(*, *) "Will write averages only."

          case("--output_file", "-o")
            call get_command_argument(iarg + 1, arg)
            output_file = arg

          case("--help", "-h")
            call usage()

          case default
            write(*, *) "Unhandled argument: '", trim(adjustl(arg)), "'."
            call usage()

        endselect
      enddo

    endif

    CONTAINS

    SUBROUTINE usage(umsg)
      ! Well... print the usage of course.
      IMPLICIT NONE
      CHARACTER(*), INTENT(in), OPTIONAL :: umsg
      write(*, *)
      write(*, *) "parse_statis: A program for extracting data from &
        &DL_POLY(_4.0) STATIS files, by Ryan James Hunt."
      write(*, *)
      write(*, *) "Usage: ./parse_statis [options]"
      write(*, *)
      write(*, *) "   [--statis_file | -s] <string>        : &
        &set input filename (default '"//trim(statis_default)//"')."
      write(*, *) "   [--output_file | -o] <string>        : &
        &set output filename (default '"//trim(output_default)//"')."
      write(*, *) "   [--labels      | -l] <int>,<int>,... : &
        &comma-separated list of integer fields to analyse."
      write(*, *) "   [--ignore      | -i] <int>           : &
        &number of lines to skip analysis for (default 0)."
      write(*, *) "   [--write-skip  | -w] <int>           : &
        &set lines skipped in output (default 0)."
      write(*, *) "   [--averages    | -a]                 : &
        &only print averages."
      write(*, *) "   [--help        | -h]                 : &
        &show this dialog."
      write(*, *)
      write(*, *) "Example: ./parse_statis -s ST1 -o PARSED -l 1,2,3 -w &
        &1000 -i 5000"
      write(*, *)
      write(*, *) "Will:"
      write(*, *) "- Read 'ST1' for the 1st, 2nd, and 3rd data &
        &entries ('engcns', 'temp', and 'engcfg')."
      write(*, *) "- Not accumulate statistics for the &
        &first 5000 data lines."
      write(*, *) "- Write every 1000th line in 'ST1' to &
        &'PARSED' (in a way that can be plotted with `xmgrace -nxy PARSED`)."
      write(*, *) "- Finally, write statistical information on every data &
        &line after the 5000th at the bottom of 'PARSED'."
      write(*, *)
      write(*, *) "The program runs line-by-line, accumulating statistics &
        &'online'. It never stores anything global from a STATIS file, and so &
        &shouldn't hog memory."
      if ( present(umsg) ) call finish(umsg)
      stop 1
    END SUBROUTINE usage

  END SUBROUTINE handle_args


  SUBROUTINE get_labels_interactive(labels_arr, uchunks)
    ! Offer user label choices interactively.
    IMPLICIT NONE
    INTEGER, ALLOCATABLE, INTENT(out) :: labels_arr(:)
    INTEGER, OPTIONAL, INTENT(in) :: uchunks
    CHARACTER(100) :: label_string = ''
    LOGICAL :: labels_ok = .false.
    INTEGER :: i, ierr, chunks = 5

    if ( present(uchunks) ) chunks = uchunks

    write(*, *)
    write(*, *) "Select data to extract:"
    write(*, *)
    write(*, *) "  [timestep no. (ntstep) and time always included]"
    write(*, *)

    ! Print label list neatly.
    do i = 1, size(label_list)
      ! Print in chunks. Manual lists these in groups of 5.
      if ( mod(i, chunks) == 0 ) then
        write(*, "(1X, I2, A4, A6, 1X)", advance = "yes") i, " -> ", adjustl(label_list(i))
      else
        write(*, "(1X, I2, A4, A6, 1X)", advance = "no") i, " -> ", adjustl(label_list(i))
      endif
    enddo
    write(*, *)
    write(*, *)
    write(*, *) "Select values to extract (comma-separated - e.g. '1,2,3')."

    do
      ! Read a big long string in.
      read(*, '(A100)', iostat = ierr) label_string

      if ( ierr == 0 ) then

        ! Try to get labels from comma-separated list.
        call delim_separated_extract(label_string, labels_arr, labels_ok)

        ! Are we done?
        if ( labels_ok ) exit

        ! If not, deallocate (if allocated) the array.
        if (allocated(labels_arr)) deallocate(labels_arr)

      endif

      ! Either ierr /=0 or the input was bad. Re-read is gonna happen. Warn.
      write(*, *) "Please enter a valid comma-separated string:"

    enddo

  END SUBROUTINE get_labels_interactive


  SUBROUTINE delim_separated_extract(string, array_ints, ok)
    ! Get an array of positive integers from a comma separated string.
    IMPLICIT NONE
    CHARACTER(*), INTENT(inout) :: string
    INTEGER, ALLOCATABLE, INTENT(inout) :: array_ints(:)
    LOGICAL, INTENT(inout) :: ok
    INTEGER n, ierr2

    n = count(transfer(string, 'a', len(string)) == ",")
    allocate(array_ints(n + 1))
    read(string, *, iostat = ierr2) array_ints(1:n+1)
    ok = ( ( ierr2 == 0 ) .and. all(array_ints > 0) )

  END SUBROUTINE delim_separated_extract


  SUBROUTINE finish(message)
    ! Wrap "stop" with an error print.
    IMPLICIT NONE
    CHARACTER(*), OPTIONAL :: message

    if ( present(message) ) then
      write(*, *)
      write(*, *) "Reason for exit: "//trim(message)//"."
      write(*, *)
      stop 1
    endif
    stop
  END SUBROUTINE finish

  SUBROUTINE transfer_header(ir, iw, nlines, width, uspacer)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: ir, iw, nlines, width
    LOGICAL, OPTIONAL, INTENT(in) :: uspacer
    LOGICAL :: spacer = .false.
    CHARACTER(10) :: hfmt
    CHARACTER(width) :: tmpline
    INTEGER i

    write(hfmt, *) "(A"//itoa(width)//")"

    ! Copy two header lines over from STATIS.
    do i = 1, nlines
      read(ir, hfmt) tmpline
      write(iw, *) "# "//trim(adjustl(tmpline))
    enddo

    ! Spacer.
    if ( present(uspacer) ) spacer = uspacer
    if ( spacer ) write(iw, *)

  END SUBROUTINE transfer_header


  SUBROUTINE write_rows_dp(iw, dat, dfmt)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: iw
    REAL(dp), INTENT(in) :: dat(:)
    CHARACTER(*), INTENT(in) :: dfmt
    INTEGER i

    ! Core of the row
    do i = 1, size(dat) - 1
      if ( iw < 0 ) then
        write(iw, dfmt, advance = "no") dat(i)
      else
        write(iw, dfmt, advance = "no") dat(i)
      endif
    enddo

    ! Last item: advance after writing.
    if ( iw < 0 ) then
      write(*, dfmt, advance = "yes") dat(i)
    else
      write(iw, dfmt, advance = "yes") dat(i)
    endif

  END SUBROUTINE write_rows_dp


  SUBROUTINE write_rows_char(iw, dat, dfmt)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: iw
    CHARACTER(*), INTENT(in) :: dat(:)
    CHARACTER(*), INTENT(in) :: dfmt
    INTEGER i

    do i = 1, size(dat) - 1
      if ( iw < 0 ) then
        write(*, dfmt, advance = "no") dat(i)
      else
        write(iw, dfmt, advance = "no") dat(i)
      endif
    enddo
    if ( iw < 0 ) then
      write(*, dfmt, advance = "yes") dat(i)
    else
      write(iw, dfmt, advance = "yes") dat(i)
    endif

  END SUBROUTINE write_rows_char


  SUBROUTINE read_statis_values(ir, dat, dfmt, chunk_size)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: ir, chunk_size
    REAL(dp), INTENT(inout) :: dat(:)
    CHARACTER(*), INTENT(in) :: dfmt
    INTEGER i, ierr

    do i = 1, size(dat)
      if ( (mod(i, chunk_size) == 0) .or. (i == size(dat)) ) then
        ! On newline!
        read(ir, dfmt, advance = "yes", iostat = ierr) dat(i)
      else
        read(ir, dfmt, advance = "no", iostat = ierr) dat(i)
      endif
      if ( ierr /= 0 ) call finish("issue reading floats in STATIS")
    enddo

  END SUBROUTINE


  SUBROUTINE filter_values(label_values, all_values, labels)
    IMPLICIT NONE
    REAL(dp), INTENT(inout) :: label_values(:)
    !REAL(dp), ALLOCATABLE, INTENT(inout) :: label_values(:)
    REAL(dp), INTENT(in) :: all_values(:)
    INTEGER, INTENT(in) :: labels(:)
    INTEGER i

    ! allocate(label_values(size(labels)))

    do i = 1, size(labels)
      label_values(i) = all_values(labels(i))
    enddo

  END SUBROUTINE filter_values


  SUBROUTINE handle_values(iw, wfmt, val)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: iw
    REAL(dp), INTENT(in) :: val(:)
    CHARACTER(*), INTENT(in) :: wfmt
    INTEGER i

    do i = 1, size(val)

      ! Do we even care about writing ...?
      if ( i == size(val) ) then
        write(iw, wfmt, advance = "yes") val(i)
      else
        write(iw, wfmt, advance = "no") val(i)
      endif

    enddo
  END SUBROUTINE handle_values


  SUBROUTINE accumulate_stats(stats, new_val, indx)
    ! "Online" update of mean and variance of a set of real data.
    !
    ! See:
    !   Pebay, P. P. Formulas for robust, one-pass parallel computation of
    !   covariances and arbitrary-order statistical moments.
    !   No. SAND2008-6212. Sandia National Laboratories, 2008.
    ! For more detailed info.
    !
    ! OR handy blog posts on this topic:
    !   https://www.johndcook.com/blog/standard_deviation/
    !   https://www.johndcook.com/blog/skewness_kurtosis/
    !
    ! NOTE: it seems there's a little bit of numerical instability here with
    ! higher moments. I've disabled printing of the skew and kurtosis elsewhere
    ! in the code, but it should be investigated.
    !
    IMPLICIT NONE
    REAL(dp), INTENT(inout) :: stats(:, :), new_val(:)
    INTEGER, INTENT(inout) :: indx
    INTEGER tmpi
    REAL(dp) tmps(size(stats, 1), 4)

    tmpi = indx
    indx = indx + 1

    tmps(:, 1) = new_val(:) - stats(:, 1)
    tmps(:, 2) = tmps(:, 1) / indx
    ! ! tmps(:, 3) = tmps(:, 2) * tmps(:, 2)
    tmps(:, 4) = tmps(:, 1) * tmps(:, 2) * tmpi

    ! Mean.
    stats(:, 1) = stats(:, 1) + tmps(:, 2);

    ! ! stats(:, 4) = stats(:, 4) + tmps(:, 4) * tmps(:, 3) * &
    ! !   &(tmpi*tmpi - 3.d0 * (tmpi + 1.d0)) + 6.d0 * tmps(:, 3) * stats(:, 2) - &
    ! !   &4.d0 * tmps(:, 2) * stats(:, 3);

    ! ! stats(:, 3) = stats(:, 3) + tmps(:, 4) * tmps(:, 2) * &
    ! !   &(tmpi - 2.d0) - 3.d0 * tmps(:, 2) * stats(:, 2);

    ! (indx - 1) * Variance
    stats(:, 2) = stats(:, 2) + tmps(:, 4);

  END SUBROUTINE accumulate_stats


  SUBROUTINE write_labels(iw, labels, num_labels)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: iw, num_labels
    INTEGER, INTENT(in) :: labels(:)
    INTEGER i

    ! Write labels across the top.
    if ( .not. g_avg_only ) then
      write(iw, *)
      write(iw, "(1X, A1, A9, A14)", advance = "no") &
        &"#", adjustr("ntstep"), adjustr("time")
    else
      write(iw, "(1X, A1, A23)", advance = "no") "#", adjustr("Averages only:")
    endif

    do i = 1, num_labels - 1
      ! If label number exceeds the size of the stored label, still try to print
      ! it. Just don't try to name it. Print the integer instead. This way, we
      ! can access unsupported ranges of data which I haven't named (e.g. stress
      ! tensor components) manually.
      if ( labels(i) .lt. size(label_list) ) then
        ! Don't trim. Want the whitespace padding out the labels.
        write(iw, "(A20)", advance = "no") adjustr(label_list(labels(i)))
      else
        write(iw, "(I20)", advance = "no") labels(i)
      endif
    enddo

    ! Last label: same as above, but advance after writing.
    if ( labels(num_labels) .lt. size(label_list) ) then
      write(iw, "(A20)", advance = "yes") adjustr(label_list(labels(num_labels)))
    else
      write(iw, "(I20)", advance = "yes") labels(size(labels))
    endif
  END SUBROUTINE write_labels


  SUBROUTINE write_stats(iw, stats, n_samples)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: iw, n_samples
    REAL(dp), INTENT(in) :: stats(:, :)

    ! Turning these off for now, need to test. Suspect numerically unstable.
    LOGICAL :: higher_moments = .false.
    INTEGER i

    write(iw, "(1X, A1, A23)", advance = "no") "#", "Mean values:"
    call write_rows(iw, stats(:, 1), output_write_fmt)

    write(iw, "(1X, A1, A23)", advance = "no") "#", "Variances:"

    ! Variance sum must be divided by (n_samples - 1) for estimate of s^2.
    call write_rows(iw, stats(:, 2) / (n_samples - 1), output_write_fmt)

    write(iw, "(1X, A1, A23)", advance = "no") "#", "Std. Devs.:"

    ! Std. dev. is sqrt of variance (w/ Bessel's correction already in place).
    call write_rows(iw, sqrt(stats(:, 2) / (n_samples - 1)), output_write_fmt)

    if ( higher_moments ) then
      write(iw, "(1X, A1, A23)", advance = "no") "#", "Skew:"
      ! Don't ask... Feel free to verify.
      call write_rows(iw, &
        &(sqrt(dble(n_samples)) * stats(:, 3)) / (stats(:, 2) ** 1.5d0), &
        &output_write_fmt)

      write(iw, "(1X, A1, A23)", advance = "no") "#", "Kurtosis:"
      ! Don't ask... Feel free to verify.
      call write_rows(iw, &
        &(dble(n_samples) * stats(:, 4) / (stats(:, 2) ** 2.d0)) - 3.d0, &
        &output_write_fmt)
    endif

    ! Print formatted errors info: may be off in cases of extreme accuracy, or
    ! for LARGE exact numbers.
    write(iw, "(1X, A1, A23)", advance = "no") "#", "Formatted errors:"
    do i = 1, size(stats, 1)
      write(iw, "(A20)", advance = "no") adjustr(&
        &formatted_error(stats(i, 1), sqrt(stats(i, 2)/(n_samples - 1)), 2))
    enddo

    write(iw, *)
    write(iw, *) "#"

    if ( g_ignore_lines > 0 ) then
      write(iw, *) "# Ignored "//trim(itoa(g_ignore_lines))//&
        &" lines of STATIS file in forming the above statistics, "//&
        &"and did NOT reblock."
    endif

  END SUBROUTINE write_stats


  FUNCTION itoa(i) RESULT(is)
    CHARACTER(:), ALLOCATABLE :: is
    INTEGER, INTENT(in) :: i
    CHARACTER(range(i) + 2) :: tmp
    write(tmp, '(i0)') i
    is = trim(tmp)
  END FUNCTION


  FUNCTION trim_zeros(r) RESULT(rt)
    REAL(dp), INTENT(in) :: r
    CHARACTER(:), ALLOCATABLE :: rt
    CHARACTER(20) :: str
    INTEGER :: ii, is = 0
    write(str, '(f20.12)') r
    str = trim(adjustl(str))
    do ii = len_trim(str), 1, -1
      if ( str(ii:ii) /= "0" ) then
        ! Avoid printing, e.g. "1." instead of "1.0". We want some precision...
        if ( str(ii:ii) == '.' ) is = 1
        exit
      endif
    enddo
    rt = str(1:ii + is)
  END FUNCTION trim_zeros


  FUNCTION formatted_error(x, dx, ns_fig) RESULT(xdxs)
    IMPLICIT NONE
    REAL(dp), INTENT(in) :: x, dx
    INTEGER, INTENT(in) :: ns_fig
    CHARACTER(:), ALLOCATABLE :: xdxs
    CHARACTER(80 + ns_fig) :: char_x, char_fmt, char_dx
    INTEGER oom
    REAL(dp) :: resc_factor, resc_x, resc_dx, tol = 1.d-5

    if ( dx < 0.d0 ) then
      call finish("found a negative standard deviation")
    endif

    ! Are we exact, in some sense? Check against tiny().
    if ( abs(dx) <= tiny(1.d0) ) then
      if ( abs(x) <= tiny(1.d0) ) then
        xdxs = "0 (0; ex)"
      else
        write(char_x, *) trim_zeros(x) // " (ex)"
        xdxs = trim(adjustl(char_x))
      endif
      return
    endif

    ! Do we want a fixed number of places, or have we fed in 0...?
    ! If so, print in scientific notation with a \pm symbol.
    if ( ns_fig == 0 ) then
      write(char_x, '(ES14.6)') x
      write(char_dx, '(ES14.6)') dx
      xdxs = trim(adjustl(char_x)) // ' ± ' // trim(adjustl(char_dx))
      return
    endif

    oom = ns_fig + floor(-log10(dx))
    resc_factor = 10.d0 ** oom
    resc_dx = anint(resc_factor * dx)

    if ( abs(resc_dx / 10.d0**ns_fig - 1.d0) < tol ) then
      oom = oom - 1
      resc_factor = resc_factor * 0.1d0
      resc_dx = resc_dx * 0.1d0
    endif

    if ( oom < 0 ) resc_dx = resc_dx * 10.d0**(-oom)
    resc_x = anint(resc_factor * x) / resc_factor

    if ( oom > 0 ) then
      char_fmt = '(f' // trim(itoa(oom + 9)) // '.' // trim(itoa(oom)) // ')'
      write(char_x, char_fmt) resc_x
    else
      char_x = trim(itoa(int(resc_x)))
    endif

    write(char_dx, '(f' // trim(itoa(ns_fig + 9)) // '.0)') resc_dx
    char_dx = adjustl(char_dx)
    char_dx = char_dx(1:len_trim(char_dx) - 1)

    xdxs = trim(adjustl(char_x)) // '(' // trim(char_dx) // ')'

  END FUNCTION formatted_error

END MODULE utils


PROGRAM main
  USE utils
  IMPLICIT NONE
  REAL(dp), ALLOCATABLE :: all_values(:), label_values(:), val_stats(:, :)
  INTEGER, ALLOCATABLE :: labels(:)
  CHARACTER(80) :: statis_file, output_file
  INTEGER :: ierr, tstep, ntstep = 0, statis_size, num_labels
  REAL(dp) :: time
  LOGICAL :: got_labels = .false., statis_exist, extent_warn = .false.

  call handle_args(statis_file, output_file, got_labels, labels)

  inquire (file = statis_file, exist = statis_exist)
  if ( .not. statis_exist ) call finish("STATIS file doesn't exist")

  if ( .not. got_labels ) then
    ! Get some...!
    call get_labels_interactive(labels)
  endif

  ! Open statis and output files
  open(1, file = statis_file, status = "old", action = "read")

  ! Could append, but struggling to think of why would ever want to...
  open(2, file = output_file, status = "replace", action = "write")

  ! Copy two header lines from statis_file to output_file, with spacer.
  call transfer_header(1, 2, 2, 80, .true.)

  ! How many things are we writing? A useful iteration counter.
  num_labels = size(labels)

  if ( any(labels > size(label_list)) ) then
    write(*, *) "WARNING: Label too big - no known named label for this data. &
      &Will print as integer."
  endif

  call write_labels(2, labels, num_labels)

  ! NB STATIS only prints the time steps the user requests in CONTROL, so our
  ! "ntstep" might be off. If you want more precise stats, then you
  ! will need to write more timesteps. If you write every nth time step, you
  ! may reduce serial correlation, but if this isn't particularly prevalent, you
  ! may also reduce the amount of data you're averaging over.

  ! "Online" mean, variance, skewness, kurtosis (no kill like overkill...!).
  allocate(val_stats(num_labels, 4)) ; val_stats = 0.d0
  allocate(label_values(size(labels))) ; label_values = 1337.13371d0

  ! Do the work: read over all timesteps.
  do
    ! Read timestep header into timestep, absolute time, and the size of the
    ! remaining statis data array.
    read(1, "(I10, E14.6, I10)", iostat = ierr) tstep, time, statis_size

    ! Exit if there are no more timesteps, or if we encounter any other read
    ! error in the above.
    if ( ierr /= 0 ) exit

    ! Set extent warning if find a label outside of statis_size.
    if ( any(labels > statis_size) ) then
      if ( .not. extent_warn ) extent_warn = .true.
    endif

    ! Get all numerical values from statis into all_values() array.
    ! Obvious default in case of unexpected issues.
    allocate(all_values(statis_size)) ; all_values = 1337.13372d0

    ! Get values... Chunks of 5.
    call read_statis_values(1, all_values, statis_read_fmt, 5)

    ! Filter out the values we actually want.
    call filter_values(label_values, all_values, labels)
    deallocate(all_values)

    ! Write values.
    if ( (.not. g_avg_only) .and. (mod(tstep, g_write_skip) == 0) ) then
      write(2, "(1X, I10, ES14.6)", advance = "no") tstep, time
      call write_rows(2, label_values, output_write_fmt)
    endif

    ! Do online stats accumulation step. Will increment ntstep.
    if ( tstep > g_ignore_lines ) &
      &call accumulate_stats(val_stats, label_values, ntstep)

  enddo

  ! These will be obscured by data at the bottom of the file - re-write them!
  if ( .not. g_avg_only ) call write_labels(2, labels, num_labels)

  ! Write averages
  call write_stats(2, val_stats, ntstep)
  if ( allocated(val_stats) ) deallocate(val_stats)

  ! Write warning, if tripped.
  ! No hashtag - want this to be apparent if try plotting file.
  ! This will trip the "Can not yet use strings..." warning of xmgrace!
  if ( extent_warn ) then
    write(2, *)
    write(2, *) "WARNING: an issue with data extents was noticed in this run."
  endif

  call finish()

END PROGRAM main
