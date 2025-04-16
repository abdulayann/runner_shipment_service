package com.dpw.runner.shipment.services.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.dto.section.request.SectionDetailsRequest;
import com.dpw.runner.shipment.services.exception.exceptions.SectionDetailsException;
import com.dpw.runner.shipment.services.service.interfaces.ISectionDetailsService;
import java.util.ArrayList;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

@ContextConfiguration(classes = {SectionDetailsController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SectionDetailsControllerTest {

  @Mock
  private ISectionDetailsService iSectionDetailsService;

  @InjectMocks
  private SectionDetailsController sectionDetailsController;

  /**
   * Method under test: {@link SectionDetailsController#create(SectionDetailsRequest)}
   */
  @Test
  void testGetAll() {
    var responseEntity = sectionDetailsController.getAll();
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testCreate() {
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionFieldIds(new ArrayList<>());
    sectionDetailsRequest.setSectionName("Section Name");
    var responseEntity = sectionDetailsController.create(sectionDetailsRequest);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testCreateException() {
    when(iSectionDetailsService.create(any())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionFieldIds(new ArrayList<>());
    sectionDetailsRequest.setSectionName("Section Name");
    var responseEntity = sectionDetailsController.create(sectionDetailsRequest);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testUpdate() {
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionFieldIds(new ArrayList<>());
    sectionDetailsRequest.setSectionName("Section Name");
    var responseEntity = sectionDetailsController.update(sectionDetailsRequest);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testUpdateException() {
    when(iSectionDetailsService.update(any())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionFieldIds(new ArrayList<>());
    sectionDetailsRequest.setSectionName("Section Name");
    var responseEntity = sectionDetailsController.update(sectionDetailsRequest);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testDelete() {
    var responseEntity = sectionDetailsController.delete(1L);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testDeleteException() {
    when(iSectionDetailsService.delete(any())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    var responseEntity = sectionDetailsController.delete(1L);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }

  @Test
  void testGetById() {
    var responseEntity = sectionDetailsController.getById(1L);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
  }

  @Test
  void testGetByIdException() {
    when(iSectionDetailsService.getById(any())).thenThrow(
        new SectionDetailsException("RuntimeException"));
    var responseEntity = sectionDetailsController.getById(1L);
    assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
  }
}
