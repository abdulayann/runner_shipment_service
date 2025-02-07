package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionFieldsRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISectionFieldsService;
import javax.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/section-fields")
public class SectionFieldsController {

  @Autowired
  private ISectionFieldsService service;

  @PostMapping
  public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid SectionFieldsRequest request) {
    try {
      request.setId(null);
      return ResponseHelper.buildSuccessResponse(service.create(request));
    } catch (Exception e) {
      return ResponseHelper.buildFailedResponse(e.getMessage());
    }

  }

  @PutMapping("/{id}")
  public ResponseEntity<IRunnerResponse> update(
      @RequestBody @Valid SectionFieldsRequest request) {
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

}

