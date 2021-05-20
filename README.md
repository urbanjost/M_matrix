# M_matrix

This module allows you to interact with a Fortran program using Matlab
or Octave-like commands. This allows you to add code to your Fortran
program that you can use to optionally inspect data during development
and debugging and to create configuration and data files that resemble
Fortran syntax. It is a WIP (Work In Progress) but is already useful.

A stand-alone program is included that lets you use it as a calculator
and to test and create input files as well.

It was originally based on some **very** old code that still requires
some major refactoring, but if anyone else is interested or finds it
useful let me know.

Perhaps it could be useful for creating a testing framework for Linear
Algebra libraries and other routines in the growing Fortran stdlib effort
or `fpm` packages as well.

Any feedback would be appreciated. How important is it to be compatible
with Matlab or Octave or Fortran? Does this appear useful for creating
configuration files, input files, inspecting data in existing programs,
transferring data between programs or providing for unit testing and
macro timing and debugging?

I do not plan on extending this to allow for creating functions at this
point, which would be required to turn it into a more powerful program,
but do plan on making it easy to extend with custom math-based procedures
and using modern Fortran features to make it more maintainable.

My primary interest is in making it into a tool for interacting with
Fortran programs.

Installation requires fpm(1):

download the github repository and build it with 
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )
   
```bash
   git clone https://github.com/urbanjost/M_matrix.git
   cd M_matrix
   fpm run
```
   
or if calling it from your `fpm` project just list it as a dependency in
the fpm.toml project file.
   
```toml
        [dependencies]
        M_matrix        = { git = "https://github.com/urbanjost/M_matrix.git" }
```

##DOCUMENTATION

 - An [index](https://urbanjost.github.io/M_strings/man3.html) to HTML versions
   of the manpages included in the distribution.
---
[CHANGELOG](CHANGELOG.md)
---
---

### Example input files:

#### avg
```text
            for i = 2:2:n, for j = 2:2:n, t = (a(i-1,j-1)+a(i-1,j)+a(i,j-1)+a(i,j))/4; ...
               a(i-1,j-1) = t; a(i,j-1) = t; a(i-1,j) = t; a(i,j) = t;
```

#### cdiv
```text
            // ======================================================
            // cdiv
            a=sqrt(random(8)
               ar = real(a); ai = imag(a); br = real(b); bi = imag(b);
               p = bi/br;
               t = (ai - p*ar)/(br + p*bi);
               cr = p*t + ar/br;
               ci = t;
               p2 = br/bi;
               t2 = (ai + p2*ar)/(bi + p2*br);
               ci2 = p2*t2 - ar/bi;
               cr2 = t2;
               s = abs(br) + abs(bi);
               ars = ar/s;
               ais = ai/s;
               brs = br/s;
               bis = bi/s;
               s = brs**2 + bis**2;
               cr3 = (ars*brs + ais*bis)/s;
               ci3 = (ais*brs - ars*bis)/s;
               <cr ci; cr2 ci2; cr3 ci3>
            // ======================================================
```

#### exp
```text
            t = 0*x + eye; s = 0*eye(x); n = 1;
            while abs(s+t-s) > 0, s = s+t, t = x*t/n, n = n + 1
```

#### four
```text
            n
            pi = 4*atan(1);
            i = sqrt(-1);
            w = exp(2*pi*i/n);
            F = <>;
            for k = 1:n, for j = 1:n, F(k,j) = w**((j-1)*(k-1));
            F = F/sqrt(n);
            alfa = r*pi;
            rho = exp(i*alfa);
            S = log(rho*F)/i - alfa*EYE;
            serr = norm(imag(S),1);
            S = real(S);
            serr = serr + norm(S-S',1)
            S = (S + S')/2;
            ferr = norm(F-exp(i*S),1)
```

#### gs
```text
            for k = 1:n, for j = 1:k-1, d = x(k,:)*x(j,:)'; x(k,:) = x(k,:) - d*x(j,:); ...
               end, s = norm(x(k,:)), x(k,:) = x(k,:)/s;
```

#### jacobi
```text
            <n, n> = size(A);
            X = eye(n);
            anorm = norm(A,'fro');
            cnt = 1;
            while cnt > 0,...
              cnt = 0;...
              for p = 1:n-1,...
                for q = p+1:n,...
                  if anorm + abs(a(p,q)) > anorm,...
                    cnt = cnt + 1;...
                    exec('jacstep');...
                  end,...
                end,...
              end,...
              display(rat(A)),...
            end
```

#### jacstep
```text
               d = (a(q,q)-a(p,p))*0.5/a(p,q);
               t = 1/(abs(d)+sqrt(d*d+1));
               if d < 0, t = -t; end;
               c = 1/sqrt(1+t*t);  s = t*c;
               R = eye(n); r(p,p)=c; r(q,q)=c; r(p,q)=s; r(q,p)=-s;
               X = X*R;
               A = R'*A*R;
```

#### kron
```text
            //  C = Kronecker product of A and B
            <m, n> = size(A);
            for i = 1:m, ...
               ci = a(i,1)*B; ...
               for j = 2:n, ci = <ci a(i,j)*B>; end ...
               if i = 1, C = ci; else, C = <C; ci>;
```

#### lanczos
```text
            <n,n> = size(A);
            q1 = rand(n,1);
            ort
            alfa = <>; beta = <>;
            q = q1/norm(q1); r = A*q(:,1);
            for j = 1:n, exec('lanstep',0);
```

#### lanstep
```text
            alfa(j) = q(:,j)'*r;
            r = r - alfa(j)*q(:,j);
            if ort <> 0, for k = 1:j-1, r = r - r'*q(:,k)*q(:,k);
            beta(j) = norm(r);
            q(:,j+1) = r/beta(j);
            r = A*q(:,j+1) - beta(j)*q(:,j);
            if j > 1, T = diag(beta(1:j-1),1); T = diag(alfa) + T + T'; eig(T)
```

#### mgs
```text
            for k = 1:n, s = norm(x(k,:)), x(k,:) = x(k,:)/s; ...
               for j = k+1:n, d = x(j,:)*x(k,:)'; x(j,:) = x(j,:) - d*x(k,:);
```

#### net
```text
            C = <
            1 2 15 . . .
            2 1 3 . . .
            3 2 4 11 . .
            4 3 5 . . .
            5 4 6 7 . .
            6 5 8 . . .
            7 5 9 30 . .
            8 6 9 10 11 .
            9 7 8 30 . .
            10 8 12 30 31 34
            11 3 8 12 13 .
            12 10 11 34 36 .
            13 11 14 . . .
            14 13 15 16 38 .
            15 1 14 . . .
            16 14 17 20 35 37
            17 16 18 . . .
            18 17 19 . . .
            19 18 20 . . .
            20 16 19 21 . .
            21 20 22 . . .
            22 21 23 . . .
            23 22 24 35 . .
            24 23 25 39 . .
            25 24 . . . .
            26 27 33 39 . .
            27 26 32 . . .
            28 29 32 . . .
            29 28 30 . . .
            30 7 9 10 29 .
            31 10 32 . . .
            32 27 28 31 34 .
            33 26 34 . . .
            34 10 12 32 33 35
            35 16 23 34 36 .
            36 12 35 38 . .
            37 16 38 . . .
            38 14 36 37 . .
            39 24 26 . . .
            >;
            <n, m> = size(C);
            A = 0*ones(n,n);
            for i=1:n, for j=2:m, k=c(i,j); if k>0, a(i,k)=1;
            check = norm(A-A',1), if check > 0, quit
            <X,D> = eig(A+eye);
            D = diag(D);  D = D(n:-1:1)
            X = X(:,n:-1:1);
            <x(:,1)/sum(x(:,1)) x(:,2) x(:,3) x(:,19)>
```

#### pascal
```text
            //Generate next Pascal matrix
            <k,k> = size(L);
            k = k + 1;
            L(k,1:k) = <L(k-1,:) 0> + <0 L(k-1,:)>;
```

#### pdq
```text
            alfa = <>; beta = 0; q = <>; p = p(:,1)/norm(p(:,1));
            t = A'*p(:,1);
            alfa(1) = norm(t);
            q(:,1) = t/alfa(1);
            X = p(:,1)*(alfa(1)*q(:,1))'
            e(1) = norm(A-X,1)
            for j = 2:r, exec('pdqstep',ip); ...
               X = X + p(:,j)*(alfa(j)*q(:,j)+beta(j)*q(:,j-1))', ...
               e(j) = norm(A-X,1)
```

#### pdqstep
```text
            t = A*q(:,j-1) - alfa(j-1)*p(:,j-1);
               if ort>0, for i = 1:j-1, t = t - t'*p(:,i)*p(:,i);
            beta(j) = norm(t);
            p(:,j) = t/beta(j);
            t = A'*p(:,j) - beta(j)*q(:,j-1);
               if ort>0, for i = 1:j-1, t = t - t'*q(:,i)*q(:,i);
            alfa(j) = norm(t);
            q(:,j) = t/alfa(j);
```

#### pop
```text
            y = < 75.995   91.972  105.711  123.203   ...
                 131.669  150.697  179.323  203.212>'
            t = < 1900:10:1970 >'
            t = (t - 1940*ones(t))/40;   <t y>
            n = 8;  A(:,1) = ones(t);  for j = 2:n, A(:,j) = t .* A(:,j-1);
            A
            c = A\y
```

#### qr
```text
            scale = s(m);
            sm = s(m)/scale; smm1 = s(m-1)/scale; emm1 = e(m-1)/scale;
            sl = s(l)/scale; el = e(l)/scale;
            b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2;
            c = (sm*emm1)**2;
            shift = sqrt(b**2+c); if b < 0, shift = -shift;
            shift = c/(b + shift)
            f = (sl + sm)*(sl-sm) - shift
            g = sl*el
            for k = l: m-1, exec('qrstep',ip)
            e(m-1) = f
```

#### qrstep
```text
            exec('rot');
            if k <> l, e(k-1) = f
            f = cs*s(k) + sn*e(k)
            e(k) = cs*e(k) - sn*s(k)
            g = sn*s(k+1)
            s(k+1) = cs*s(k+1)
            exec('rot');
            s(k) = f
            f = cs*e(k) + sn*s(k+1)
            s(k+1) = -sn*e(k) + cs*s(k+1)
            g = sn*e(k+1)
            e(k+1) = cs*e(k+1)
```

#### rho
```text
            //Conductivity example.
            //Parameters ---
               rho       //radius of cylindrical inclusion
               n         //number of terms in solution
               m         //number of boundary points
            //initialize operation counter
               flop = <0 0>;
            //initialize variables
               m1 = round(m/3);   //number of points on each straight edge
               m2 = m - m1;       //number of points with Dirichlet conditions
               pi = 4*atan(1);
            //generate points in Cartesian coordinates
               //right hand edge
               for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);
               //top edge
               for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;
               //circular edge
               for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...
                  x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);
            //convert to polar coordinates
               for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...
                  r(i) = sqrt(x(i)**2+y(i)**2);
               th(m) = pi/2;  r(m) = 1;
            //generate matrix
               //Dirichlet conditions
               for i = 1:m2, for j = 1:n, k = 2*j-1; ...
                  a(i,j) = r(i)**k*cos(k*th(i));
               //Neumann conditions
               for i = m2+1:m, for j = 1:n, k = 2*j-1; ...
                  a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));
            //generate right hand side
               for i = 1:m2, b(i) = 1;
               for i = m2+1:m, b(i) = 0;
            //solve for coefficients
               c = A$b
            //compute effective conductivity
               c(2:2:n) = -c(2:2:n)
               sigma = sum(c)
            //output total operation count
               ops = flop(2)
```

#### rogers.exec
```text
            exec('d.boug');                        // reads data
            <g,k> = size(p);               // p is matrix of gene frequencies
            wv = ncen/sum(ncen);           // ncen contains population sizes
            pbar = wv*p;                   // weighted average of p
            p = p - ones(g,1)*pbar;        // deviations from mean
            p = sqrt(diag(wv)) * p;        // weight rows of p by sqrt of pop size
            h = diag(pbar); h = h*(eye-h); // diagonal contains binomial variance: p*(1-p)
            r = p*inv(h)*p'/k;             // normalized covariance matrix
            eig(r)'
```

#### rosser
```text
            A  = <
              611.  196. -192.  407.   -8.  -52.  -49.   29.
              196.  899.  113. -192.  -71.  -43.   -8.  -44.
             -192.  113.  899.  196.   61.   49.    8.   52.
              407. -192.  196.  611.    8.   44.   59.  -23.
               -8.  -71.   61.    8.  411. -599.  208.  208.
              -52.  -43.   49.   44. -599.  411.  208.  208.
              -49.   -8.    8.   59.  208.  208.   99. -911.
               29.  -44.   52.  -23.  208.  208. -911.   99.  >;
```

#### rot
```text
            // subexec rot(f,g,cs,sn)
                  rho = g; if abs(f) > abs(g), rho = f;
                  cs = 1.0; sn = 0.0; z = 1.0;
                  r = norm(<f g>); if rho < 0, r = -r; r
                  if r <> 0.0, cs = f/r
                  if r <> 0.0, sn = g/r
                  if abs(f) > abs(g), z = sn;
                  if abs(g) >= abs(f), if cs <> 0, z = 1/cs;
                  f = r;
                  g = z;
```
#### rqi
```text
            rho = (x'*A*x)
            x = (A-rho*eye)\x;
            x = x/norm(x)
```
#### setup
```text
            diary('xxx')
            !tail -f xxx > /dev/tty1 &
            !tail -f xxx > /dev/tty2 &
```
#### sigma
```text
            RHO = .5  M = 20  N = 10   SIGMA =  1.488934271883534
            RHO = .5  M = 40  N = 20   SIGMA =  1.488920312974229
            RHO = .5  M = 60  N = 30   SIGMA =  1.488920697912116
```
#### strut.mat
```text
            // Structure problem, Forsythe, Malcolm and Moler, p. 62
            s =  sqrt(2)/2;
            A = <
            -s  .  .  1  s   .  .  .  .  .  .  .  .  .  .  .  .
            -s  . -1  . -s   .  .  .  .  .  .  .  .  .  .  .  .
             . -1  .  .  .   1  .  .  .  .  .  .  .  .  .  .  .
             .  .  1  .  .   .  .  .  .  .  .  .  .  .  .  .  .
             .  .  . -1  .   .  .  1  .  .  .  .  .  .  .  .  .
             .  .  .  .  .   . -1  .  .  .  .  .  .  .  .  .  .
             .  .  .  . -s -1  .  .  s  1  .  .  .   .  .  .  .
             .  .  .  .  s   .  1  .  s  .  .  .  .  .  .  .  .
             .  .  .  .  .   .  . -1 -s  .  .  1  s  .  .  .  .
             .  .  .  .  .   .  .  . -s  . -1  . -s  .  .  .  .
             .  .  .  .  .   .  .  .  . -1  .  .  .  1  .  .  .
             .  .  .  .  .   .  .  .  .  .  1  .  .  .  .  .  .
             .  .  .  .  .   .  .  .  .  .  . -1  .  .  .  s  .
             .  .  .  .  .   .  .  .  .  .  .  .  .  . -1 -s  .
             .  .  .  .  .   .  .  .  .  .  .  . -s -1  .  .  1
             .  .  .  .  .   .  .  .  .  .  .  .  s  .  1  .  .
             .  .  .  .  .   .  .  .  .  .  .  .  .  .  . -s -1>;
            b = <
             .  .  . 10  .   .  . 15  .  .  .  .  .  .  . 10  .>';
```
#### test1
```text
            // -----------------------------------------------------------------
            // start a new log file
            sh rm -fv log.txt
            diary('log.txt')
            // -----------------------------------------------------------------
            titles=<'GNP deflator'
             'GNP         '
             'Unemployment'
             'Armed Force '
             'Population  '
             'Year        '
             'Employment  '>;
            data = ...
            < 83.0  234.289  235.6  159.0  107.608  1947  60.323
              88.5  259.426  232.5  145.6  108.632  1948  61.122
              88.2  258.054  368.2  161.6  109.773  1949  60.171
              89.5  284.599  335.1  165.0  110.929  1950  61.187
              96.2  328.975  209.9  309.9  112.075  1951  63.221
              98.1  346.999  193.2  359.4  113.270  1952  63.639
              99.0  365.385  187.0  354.7  115.094  1953  64.989
             100.0  363.112  357.8  335.0  116.219  1954  63.761
             101.2  397.469  290.4  304.8  117.388  1955  66.019
             104.6  419.180  282.2  285.7  118.734  1956  67.857
             108.4  442.769  293.6  279.8  120.445  1957  68.169
             110.8  444.546  468.1  263.7  121.950  1958  66.513
             112.6  482.704  381.3  255.2  123.366  1959  68.655
             114.2  502.601  393.1  251.4  125.368  1960  69.564
             115.7  518.173  480.6  257.2  127.852  1961  69.331
             116.9  554.894  400.7  282.7  130.081  1962  70.551>;
            short
            X = data;
            <n,p> = size(X)
            mu = ones(1,n)*X/n
            X = X - ones(n,1)*mu;  X = X/diag(sqrt(diag(X'*X)))
            corr = X'*X
            y = data(:,p); X = <ones(y) data(:,1:p-1)>;
            long e
            beta = X\y
            expected = < ...
               -3.482258634594421D+03
                1.506187227124484D-02
               -3.581917929257409D-02
               -2.020229803816908D-02
               -1.033226867173703D-02
               -5.110410565317738D-02
                1.829151464612817D+00
            >
            disp('EXPE and BETA should be the same')
```
#### tryall
```text
            diary('log.txt')
            a=magic(8)
            n=3
            exec('avg')
            b=random(8,8)
            exec('cdiv')
            exec('exp')
            exec('four')
            exec('gs')
            exec('jacobi')
            // jacstep
            exec('kron')
            exec('lanczos')
            // lanstep
            exec('longley')
            exec('mgs')
            exec('net')
            exec('pascal')
            exec('pdq')
            // pdqstep
            exec('pop')
            exec('qr')
            // qrstep
            exec('rho')
            exec('rosser')
            // rot
            exec('rqi')
            exec('setup')
            exec('sigma')
            exec('strut.mat')
            exec('w5')
            exec('rogers.exec
            exec('rogers.load
```
#### w5
```text
            w5    = <
                      1.          1.          0.          0.          0.
                    -10.          1.          1.          0.          0.
                     40.          0.          1.          1.          0.
                   -205.          0.          0.          1.          1.
                   1024.          0.          0.          0.         -4.
                     >
```
