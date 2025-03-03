package com.dpw.runner.shipment.services.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.exception.exceptions.SectionDetailsException;
import com.dpw.runner.shipment.services.exception.exceptions.SectionVisibilityException;
import com.dpw.runner.shipment.services.service.interfaces.ISectionVisibilityService;
import java.util.HashSet;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = {SectionVisibilityControllerTest.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SectionVisibilityControllerTest {

  @Mock
  private ISectionVisibilityService iSectionVisibilityService;

  @InjectMocks
  private SectionVisibilityController sectionVisibilityController;

  @Test
  void testGetAll() {
    var responseEntity = sectionVisibilityController.getAll();
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testCreate() {
    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Branch");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(new HashSet<>());
    var responseEntity = sectionVisibilityController.create(sectionVisibilityRequest);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testCreateException() {
    when(iSectionVisibilityService.create(any())).thenThrow(
        new SectionVisibilityException("RuntimeException"));
    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Branch");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(new HashSet<>());
    var responseEntity = sectionVisibilityController.create(sectionVisibilityRequest);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testUpdate() {
    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Branch");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(new HashSet<>());
    var responseEntity = sectionVisibilityController.update(sectionVisibilityRequest);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testUpdateException() {
    when(iSectionVisibilityService.update(any())).thenThrow(
        new SectionVisibilityException("RuntimeException"));
    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Branch");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(new HashSet<>());
    var responseEntity = sectionVisibilityController.update(sectionVisibilityRequest);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testDelete() {
    var responseEntity = sectionVisibilityController.delete(1L);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testDeleteException() {
    when(iSectionVisibilityService.delete(any())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    var responseEntity = sectionVisibilityController.delete(1L);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetById() {
    var responseEntity = sectionVisibilityController.getById(1L);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testGetByIdException() {
    when(iSectionVisibilityService.getById(any())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    var responseEntity = sectionVisibilityController.getById(1L);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testFindByBranchModeDirection() {
    var responseEntity = sectionVisibilityController.findByBranchModeDirection("branch", "SEA",
        "IMP");
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testFindByBranchModeDirectionException() {
    when(iSectionVisibilityService.findByBranchModeDirection(anyString(), anyString(),
        anyString())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    var responseEntity = sectionVisibilityController.findByBranchModeDirection("branch", "SEA",
        "IMP");
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetAllPaginated() {
    var responseEntity = sectionVisibilityController.getAllPaginated(1, 1,
        "id", "desc");
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testGetAllPaginatedException() {
    when(iSectionVisibilityService.getAllPaginated(1, 1,
        "id", "desc")).thenThrow(
        new SectionDetailsException("RuntimeException"));
    var responseEntity = sectionVisibilityController.getAllPaginated(1, 1,
        "id", "desc");
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetAllPaginatedPageNoThan1() {
    var responseEntity = sectionVisibilityController.getAllPaginated(0, 1,
        "id", "desc");
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetAllPaginatedPageSizeLessThan1() {
    var responseEntity = sectionVisibilityController.getAllPaginated(1, 0,
        "id", "desc");
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetAllPaginatedPageSizeLessGreaterThan100() {
    var responseEntity = sectionVisibilityController.getAllPaginated(1, 101,
        "id", "desc");
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }
}
