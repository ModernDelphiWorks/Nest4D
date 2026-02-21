/**
 * Main JavaScript for Nest4D Website
 * Handles all interactive functionality including navigation, animations, and user interactions
 * @author Nest4D Team
 * @version 1.0.0
 */

/**
 * Initialize all website functionality when DOM is loaded
 */
document.addEventListener('DOMContentLoaded', function () {
    initializeNavigation();
    initializeScrollAnimations();
    initializeTabSwitching();
    initializeParallaxEffects();
    initializeTypewriter();
    initializeSmoothScrolling();
    initializeThemeToggle();
    initializeMobileMenu();
    initializeCodeHighlighting();
    initializePerformanceOptimizations();
});

/**
 * Initialize navigation functionality including scroll effects and active link highlighting
 */
function initializeNavigation() {
    const navbar = document.querySelector('.navbar');
    const navLinks = document.querySelectorAll('.nav-link');

    // Navbar scroll effect
    window.addEventListener('scroll', () => {
        if (window.scrollY > 100) {
            navbar.classList.add('scrolled');
        } else {
            navbar.classList.remove('scrolled');
        }
    });

    // Active link highlighting
    const sections = document.querySelectorAll('section[id]');

    window.addEventListener('scroll', () => {
        let current = '';

        sections.forEach(section => {
            const sectionTop = section.offsetTop;

            if (window.scrollY >= (sectionTop - 200)) {
                current = section.getAttribute('id');
            }
        });

        navLinks.forEach(link => {
            link.classList.remove('active');
            if (link.getAttribute('href') === `#${current}`) {
                link.classList.add('active');
            }
        });
    });
}

/**
 * Initialize scroll-based animations using Intersection Observer API
 */
function initializeScrollAnimations() {
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -50px 0px'
    };

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('animated');

                // Add staggered animation for grid items
                if (entry.target.classList.contains('features-grid') ||
                    entry.target.classList.contains('community-grid')) {
                    const items = entry.target.children;
                    Array.from(items).forEach((item, index) => {
                        setTimeout(() => {
                            item.style.animation = `fadeInUp 0.6s ease-out forwards`;
                            item.style.animationDelay = `${index * 0.1}s`;
                        }, index * 100);
                    });
                }
            }
        });
    }, observerOptions);

    // Observe elements for animation
    const animatedElements = document.querySelectorAll(
        '.feature-item, .arch-feature, .community-card, .section-header, .hero-content'
    );

    animatedElements.forEach(el => {
        el.classList.add('animate-on-scroll');
        observer.observe(el);
    });
}

/**
 * Initialize tab switching functionality for code examples
 */
function initializeTabSwitching() {
    const tabButtons = document.querySelectorAll('.tab-btn');
    const tabPanes = document.querySelectorAll('.tab-pane');

    tabButtons.forEach(button => {
        button.addEventListener('click', () => {
            const targetTab = button.getAttribute('data-tab');

            // Remove active class from all buttons and panes
            tabButtons.forEach(btn => btn.classList.remove('active'));
            tabPanes.forEach(pane => pane.classList.remove('active'));

            // Add active class to clicked button and corresponding pane
            button.classList.add('active');
            document.getElementById(targetTab).classList.add('active');

            // Re-highlight code in the new tab
            if (window.Prism) {
                window.Prism.highlightAll();
            }
        });
    });
}

// Parallax effects
function initializeParallaxEffects() {
    const orbs = document.querySelectorAll('.gradient-orb');
    const codeWindow = document.querySelector('.code-window');
    const floatingCards = document.querySelectorAll('.feature-card');

    let ticking = false;

    function updateParallax() {
        const scrolled = window.pageYOffset;

        // Parallax orbs
        orbs.forEach((orb, index) => {
            if (orb) {
                const speed = 0.2 + (index * 0.1);
                orb.style.transform = `translateY(${scrolled * speed}px) rotate(${scrolled * 0.1}deg)`;
            }
        });

        // Code window parallax - unified transformation
        if (codeWindow) {
            const rate = scrolled * -0.1; // Simplified rate calculation
            codeWindow.style.transform = `translateY(${rate}px)`;
        }

        // Floating cards - unified movement with smooth animation
        floatingCards.forEach((card, index) => {
            if (card) {
                const baseParallax = scrolled * -0.15;
                const sineMovement = Math.sin(scrolled * 0.01 + index) * 8;
                const finalTransform = baseParallax + sineMovement;
                card.style.transform = `translateY(${finalTransform}px)`;
            }
        });

        ticking = false;
    }

    function requestTick() {
        if (!ticking) {
            requestAnimationFrame(updateParallax);
            ticking = true;
        }
    }

    window.addEventListener('scroll', requestTick, {
        passive: true
    });
}

// Typewriter effect for hero title
function initializeTypewriter() {
    const titleElement = document.querySelector('.hero-title .gradient-text');
    if (!titleElement) return;

    const text = titleElement.textContent;
    titleElement.textContent = '';

    let i = 0;
    const typeSpeed = 100;

    function typeWriter() {
        if (i < text.length) {
            titleElement.textContent += text.charAt(i);
            i++;
            setTimeout(typeWriter, typeSpeed);
        } else {
            // Add blinking cursor
            titleElement.innerHTML += '<span class="cursor">|</span>';

            // Remove cursor after animation
            setTimeout(() => {
                const cursor = titleElement.querySelector('.cursor');
                if (cursor) cursor.remove();
            }, 2000);
        }
    }

    // Start typewriter effect after a delay
    setTimeout(typeWriter, 1000);
}

// Smooth scrolling for anchor links
function initializeSmoothScrolling() {
    const links = document.querySelectorAll('a[href^="#"]');

    links.forEach(link => {
        link.addEventListener('click', (e) => {
            e.preventDefault();

            const targetId = link.getAttribute('href').substring(1);
            const targetElement = document.getElementById(targetId);

            if (targetElement) {
                const offsetTop = targetElement.offsetTop - 80; // Account for fixed navbar

                window.scrollTo({
                    top: offsetTop,
                    behavior: 'smooth'
                });
            }
        });
    });
}

// Theme toggle functionality
function initializeThemeToggle() {
    // Create theme toggle button
    const themeToggle = document.createElement('button');
    themeToggle.className = 'theme-toggle';
    themeToggle.innerHTML = '🌙';
    themeToggle.setAttribute('aria-label', 'Toggle dark mode');

    // Add to navigation
    const navActions = document.querySelector('.nav-actions');
    if (navActions) {
        navActions.insertBefore(themeToggle, navActions.firstChild);
    }

    // Check for saved theme preference
    const savedTheme = localStorage.getItem('theme');
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;

    if (savedTheme === 'dark' || (!savedTheme && prefersDark)) {
        document.body.classList.add('dark-theme');
        themeToggle.innerHTML = '☀️';
    }

    // Theme toggle event
    themeToggle.addEventListener('click', () => {
        document.body.classList.toggle('dark-theme');
        const isDark = document.body.classList.contains('dark-theme');

        themeToggle.innerHTML = isDark ? '☀️' : '🌙';
        localStorage.setItem('theme', isDark ? 'dark' : 'light');

        // Animate theme transition
        document.body.style.transition = 'background-color 0.3s ease, color 0.3s ease';
        setTimeout(() => {
            document.body.style.transition = '';
        }, 300);
    });
}

// Mobile menu functionality
function initializeMobileMenu() {
    const mobileToggle = document.querySelector('.mobile-menu-toggle');
    const navMenu = document.querySelector('.nav-menu');
    const navActions = document.querySelector('.nav-actions');

    if (!mobileToggle) return;

    mobileToggle.addEventListener('click', () => {
        mobileToggle.classList.toggle('active');
        navMenu.classList.toggle('active');
        navActions.classList.toggle('active');

        // Prevent body scroll when menu is open
        document.body.classList.toggle('menu-open');
    });

    // Close menu when clicking on a link
    const navLinks = document.querySelectorAll('.nav-link');
    navLinks.forEach(link => {
        link.addEventListener('click', () => {
            mobileToggle.classList.remove('active');
            navMenu.classList.remove('active');
            navActions.classList.remove('active');
            document.body.classList.remove('menu-open');
        });
    });
}

// Code highlighting enhancements
function initializeCodeHighlighting() {
    // Add copy buttons to code blocks
    const codeBlocks = document.querySelectorAll('pre code');

    codeBlocks.forEach(block => {
        const pre = block.parentElement;
        const copyButton = document.createElement('button');
        copyButton.className = 'copy-button';
        copyButton.innerHTML = '📋';
        copyButton.setAttribute('aria-label', 'Copy code');

        pre.style.position = 'relative';
        pre.appendChild(copyButton);

        copyButton.addEventListener('click', async () => {
            try {
                await navigator.clipboard.writeText(block.textContent);
                copyButton.innerHTML = '✅';
                copyButton.style.color = '#10b981';

                setTimeout(() => {
                    copyButton.innerHTML = '📋';
                    copyButton.style.color = '';
                }, 2000);
            } catch (err) {
                console.error('Failed to copy code:', err);
                copyButton.innerHTML = '❌';
                setTimeout(() => {
                    copyButton.innerHTML = '📋';
                }, 2000);
            }
        });
    });

    // Add line numbers to code blocks
    codeBlocks.forEach(block => {
        const lines = block.textContent.split('\n');
        if (lines.length > 5) { // Only add line numbers for longer code blocks
            block.classList.add('line-numbers');
        }
    });
}

// Performance optimizations
function initializePerformanceOptimizations() {
    // Lazy loading for images
    const images = document.querySelectorAll('img[data-src]');

    const imageObserver = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const img = entry.target;
                img.src = img.dataset.src;
                img.classList.add('loaded');
                imageObserver.unobserve(img);
            }
        });
    });

    images.forEach(img => imageObserver.observe(img));

    // Preload critical resources
    const criticalResources = [
        'assets/images/nest4d_logo.png',
        'assets/images/nest4d_fluxo.png'
    ];

    criticalResources.forEach(resource => {
        const link = document.createElement('link');
        link.rel = 'preload';
        link.as = 'image';
        link.href = resource;
        document.head.appendChild(link);
    });

    // Service Worker registration for caching
    if ('serviceWorker' in navigator) {
        window.addEventListener('load', () => {
            navigator.serviceWorker.register('/sw.js')
                .then(registration => {
                    console.log('SW registered: ', registration);
                })
                .catch(registrationError => {
                    console.log('SW registration failed: ', registrationError);
                });
        });
    }
}

// Utility functions
function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

function throttle(func, limit) {
    let inThrottle;
    return function () {
        const args = arguments;
        const context = this;
        if (!inThrottle) {
            func.apply(context, args);
            inThrottle = true;
            setTimeout(() => inThrottle = false, limit);
        }
    }
}

// Analytics and tracking
function trackEvent(eventName, properties = {}) {
    // Placeholder for analytics tracking
    console.log('Event tracked:', eventName, properties);

    // Example: Google Analytics 4
    if (typeof gtag !== 'undefined') {
        gtag('event', eventName, properties);
    }
}

// Error handling
window.addEventListener('error', (event) => {
    console.error('JavaScript error:', event.error);
    trackEvent('javascript_error', {
        message: event.message,
        filename: event.filename,
        lineno: event.lineno
    });
});

// Performance monitoring
window.addEventListener('load', () => {
    // Measure page load performance
    const perfData = performance.getEntriesByType('navigation')[0];
    const loadTime = perfData.loadEventEnd - perfData.loadEventStart;

    trackEvent('page_load_time', {
        load_time: loadTime,
        dom_content_loaded: perfData.domContentLoadedEventEnd - perfData.domContentLoadedEventStart
    });
});

// Keyboard navigation support
document.addEventListener('keydown', (e) => {
    // ESC key closes mobile menu
    if (e.key === 'Escape') {
        const mobileToggle = document.querySelector('.mobile-menu-toggle');
        const navMenu = document.querySelector('.nav-menu');

        if (navMenu && navMenu.classList.contains('active')) {
            mobileToggle.classList.remove('active');
            navMenu.classList.remove('active');
            document.body.classList.remove('menu-open');
        }
    }

    // Tab navigation for accessibility
    if (e.key === 'Tab') {
        document.body.classList.add('keyboard-navigation');
    }
});

// Remove keyboard navigation class on mouse use
document.addEventListener('mousedown', () => {
    document.body.classList.remove('keyboard-navigation');
});

// Export functions for external use
window.Nest4D = {
    trackEvent,
    debounce,
    throttle
};