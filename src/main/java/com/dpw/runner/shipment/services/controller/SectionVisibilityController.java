package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISectionVisibilityService;
import org.springframework.beans.factory.annotation.Autowired;
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
@RequestMapping("/section-visibility")
public class SectionVisibilityController {

  @Autowired
  private ISectionVisibilityService service;

  @PostMapping
  public ResponseEntity<IRunnerResponse> create(@RequestBody SectionVisibilityRequest request) {
    try {
      request.setId(null); // Ensure ID is null for new creation
      return ResponseHelper.buildSuccessResponse(service.create(request));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }
  }

  @PutMapping("/{id}")
  public ResponseEntity<IRunnerResponse> update(@RequestBody SectionVisibilityRequest request) {
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
    return ResponseHelper.buildSuccessResponse(service.getById(id));
  }

  @GetMapping
  public ResponseEntity<IRunnerResponse> getAll() {
    return ResponseEntity.ok(service.getAll());
  }

  @GetMapping("/search")
  public ResponseEntity<IRunnerResponse> findByTenantIdModeDirection(
      @RequestParam Integer tenantId,
      @RequestParam String mode,
      @RequestParam String direction) {
    return ResponseHelper.buildSuccessResponse(
        service.findByTenantIdModeDirection(tenantId, mode, direction));
  }
}
