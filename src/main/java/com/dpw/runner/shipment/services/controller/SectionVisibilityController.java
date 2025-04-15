package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISectionVisibilityService;
import javax.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("api/v3/section-visibility")
public class SectionVisibilityController {

  private final ISectionVisibilityService service;

  public SectionVisibilityController(ISectionVisibilityService service) {
    this.service = service;
  }

  @PostMapping
  public ResponseEntity<IRunnerResponse> create(
      @RequestBody @Valid SectionVisibilityRequest request) {
    try {
      request.setId(null); // Ensure ID is null for new creation
      return ResponseHelper.buildSuccessResponse(service.create(request));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }

  @PutMapping
  public ResponseEntity<IRunnerResponse> update(
      @RequestBody @Valid SectionVisibilityRequest request) {
    try {
      return ResponseHelper.buildSuccessResponse(service.update(request));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }

  @DeleteMapping("/{id}")
  public ResponseEntity<IRunnerResponse> delete(@PathVariable Long id) {
    try {
      return ResponseHelper.buildSuccessResponse(service.delete(id));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }

  @GetMapping("/{id}")
  public ResponseEntity<IRunnerResponse> getById(@PathVariable Long id) {
    try {
      return ResponseHelper.buildSuccessResponse(service.getById(id));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }

  @GetMapping
  public ResponseEntity<IRunnerResponse> getAll() {
    return ResponseEntity.ok(service.getAll());
  }

  @GetMapping("/search")
  public ResponseEntity<IRunnerResponse> findByBranchModeDirection(
      @RequestParam String branch,
      @RequestParam String mode,
      @RequestParam String direction) {
    try {
      return ResponseHelper.buildSuccessResponse(
          service.findByBranchModeDirection(branch, mode, direction));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }

  @GetMapping("/list")
  public ResponseEntity<IRunnerResponse> getAllPaginated(
      @RequestParam(defaultValue = "1") int page,
      @RequestParam(defaultValue = "10") int size,
      @RequestParam(defaultValue = "id") String sortBy,
      @RequestParam(defaultValue = "asc") String sortDirection) {
    if (page < 1) {
      return ResponseHelper.buildFailedResponse("Page number must be greater than or equal to 1.");
    }

    if (size <= 0) {
      return ResponseHelper.buildFailedResponse("Size must be greater than 0.");
    }

    // Set a maximum size limit (e.g., 100)
    if (size > 100) {
      return ResponseHelper.buildFailedResponse("Size must not exceed 100.");
    }
    try {
      return ResponseEntity.ok(service.getAllPaginated(page, size, sortBy, sortDirection));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }
}
