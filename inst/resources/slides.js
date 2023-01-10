(function(d) {
  const n = 3; // find a container that has at least n "slides"
  let p = d.body;  // container of slides; assume <body> for now
  // add 'slide' class to the frontmatter div and toc
  ['.frontmatter', '#TOC'].forEach((s) => {
    const fm = p.querySelector(s);
    fm && fm.classList.add('slide');
  });
  const s1 = ':scope > hr:not([class])', s2 = ':scope > h2';
  function findContainer(s) {
    if (p.querySelectorAll(s).length >= n) return true;
    // if body doesn't contain headers or <hr>s, look into children
    for (let i = 0; i < p.children.length; i++) {
      if (p.children[i].querySelectorAll(s).length >= n) {
        p = p.children[i];
        break;
      }
    }
    return false;
  }
  if (!findContainer(s1)) {
    // if not enough <hr>s found in children; look for <h2> instead
    if (p.tagName === 'BODY') {
      // not enough h2 found, this page is not appropriate for slides
      if (!findContainer(s2) && p.tagName === 'BODY') return;
      p.querySelectorAll(s2).forEach((h2) => {
        h2.before(d.createElement('hr'));
      });
    }
  }

  function newSlide() {
    let s = d.createElement('div')
    s.className = 'slide';
    return s;
  }
  function isSep(el) {
    return el.tagName === 'HR' && el.attributes.length === 0;
  }
  let el = p.firstElementChild;
  if (isSep(el)) el.remove();
  el = p.firstElementChild;
  if (!el) return;
  let s = newSlide();
  el.before(s);
  while (true) {
    let el = s.nextElementSibling;
    if (!el) break;
    // remove slide separators (<hr>) and create new slide
    if (isSep(el)) {
      s = newSlide();
      el.before(s);
      el.remove();
    } else if (el.classList.contains('slide')) {
      if (s.innerText === '') {
        el.after(s);
      } else {
        s = newSlide();
        el.after(s);
      }
    } else {
      s.append(el);
    }
  }
  const slides = d.querySelectorAll('div.slide'), N = slides.length;
  slides.forEach((s, i) => {
    let n = d.createElement('span');
    n.innerText = i + 1 + '/' + N
    n.className = 'page-number';
    s.append(n);
  });
  // press f for fullscreen mode
  d.addEventListener('keyup', (e) => {
    e.key === 'f' && d.documentElement.requestFullscreen();
    e.key === 'o' && d.body.classList.toggle('overview');
    e.key === 'm' && d.body.classList.toggle('mirrored');
  });
  d.querySelectorAll('a').forEach((a) => {
    if (a.childElementCount === 0) {
      a.innerText = a.innerText.replace(/^https:\/\//, '');
    }
  })
})(document);
