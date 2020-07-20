
context("Test basic usage")

set.seed(20200428)
cap <- 100
res <- scenario_sim(n.sim = 2,
                     num.initial.cases = 1,
                     cap_max_days = 100,
                     cap_cases = cap,
                     r0isolated = 0,
                     r0community = 0,
                     disp.iso = 1,
                     disp.com = 0.16,
                     delay_shape = 2.5,
                     delay_scale = 5,
                     inc_meanlog = 1.434065,
                     inc_sdlog = 0.6612,
                     inf_shape = 2.115779,
                     inf_rate = 0.6898583,
                     inf_shift = 3,
                     min_quar_delay = 1,
                     max_quar_delay = 3,
                     self_report = 0.5,
                     precaution = 1,
                     test_delay = 1,
                     quarantine = TRUE,
                     prop.asym = 0,
                     sensitivity = 0.9, 
                     prop.ascertain = 0)

test_that("A basic sim returns the correct object", {

  # Check we got 2 simulations as requested
  expect_true(length(unique(res$sim)) == 2)

  expect_true(inherits(res, 'data.frame'))
  expect_true(inherits(res, 'data.table'))

  # Test that weeks increase.
  # As we have n.sim = 2, exactly 1 sim should be smaller than the previous week
  # Same for cumulative cases.
  expect_equal(sum((res$week[seq(2,nrow(res))] - res$week[seq(nrow(res) - 1)]) < 0), 1)
  expect_equal(sum((res$cumulative[seq(2,nrow(res))] - res$cumulative[seq(nrow(res) - 1)]) < 0), 1)

  
  
  
})
