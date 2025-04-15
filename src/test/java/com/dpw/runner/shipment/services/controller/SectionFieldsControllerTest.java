package com.dpw.runner.shipment.services.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.dto.section.request.SectionFieldsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.SectionFieldsException;
import com.dpw.runner.shipment.services.service.interfaces.ISectionFieldsService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = {SectionFieldsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SectionFieldsControllerTest {

  @Mock
  private ISectionFieldsService sectionFieldsService;

  @InjectMocks
  private SectionFieldsController sectionFieldsController;

  @Test
  void testGetAll() {
    var responseEntity = sectionFieldsController.getAll();
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testCreate() {
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    var responseEntity = sectionFieldsController.create(sectionFieldsRequest);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testCreateException() {
    when(sectionFieldsService.create(any())).thenThrow(
        new SectionFieldsException("RuntimeException"));
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    var responseEntity = sectionFieldsController.create(sectionFieldsRequest);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testUpdate() {
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    var responseEntity = sectionFieldsController.update(sectionFieldsRequest);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testUpdateException() {
    when(sectionFieldsService.update(any())).thenThrow(
        new SectionFieldsException("RuntimeException"));
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    var responseEntity = sectionFieldsController.update(sectionFieldsRequest);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testDelete() {
    var responseEntity = sectionFieldsController.delete(1L);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testDeleteException() {
    when(sectionFieldsService.delete(any())).thenThrow(
        new SectionFieldsException("RuntimeException"));
    var responseEntity = sectionFieldsController.delete(1L);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetById() {
    var responseEntity = sectionFieldsController.getById(1L);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testGetByIdException() {
    when(sectionFieldsService.getById(any())).thenThrow(
        new SectionFieldsException("RuntimeException"));
    var responseEntity = sectionFieldsController.getById(1L);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }
}
