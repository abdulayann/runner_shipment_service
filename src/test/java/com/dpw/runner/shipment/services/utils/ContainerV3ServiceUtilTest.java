package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.entity.Containers;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerV3ServiceUtilTest {

  @InjectMocks
  private ContainerValidationUtil containerValidationUtil;

  @Test
  void testValidateUpdateBulkRequest_withValidRequests_shouldPass() {
    List<ContainerV3Request> requests = List.of(
        ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).build(),
        ContainerV3Request.builder().id(2L).containerCode("Code").commodityGroup("FCR").containerCount(2L).build()
    );

    assertDoesNotThrow(() -> containerValidationUtil.validateUpdateBulkRequest(requests));
  }

  @Test
  void testValidateUpdateBulkRequest_withNullList_shouldThrowException() {
    Exception exception = assertThrows(IllegalArgumentException.class,
        () -> containerValidationUtil.validateUpdateBulkRequest(null));

    assertEquals("Bulk update request cannot be null or empty.", exception.getMessage());
  }

  @Test
  void testValidateUpdateBulkRequest_withEmptyList_shouldThrowException() {
    Exception exception = assertThrows(IllegalArgumentException.class,
        () -> containerValidationUtil.validateUpdateBulkRequest(List.of()));

    assertEquals("Bulk update request cannot be null or empty.", exception.getMessage());
  }

  @Test
  void testValidateUpdateBulkRequest_withNullId_shouldThrowException() {
    List<ContainerV3Request> requests = List.of(
        ContainerV3Request.builder().id(1L).containerCode("Code").commodityGroup("FCR").containerCount(2L).build(),
        ContainerV3Request.builder().id(null).containerCode("Code").commodityGroup("FCR").containerCount(2L).build()
    );

    Exception exception = assertThrows(IllegalArgumentException.class,
        () -> containerValidationUtil.validateUpdateBulkRequest(requests));

    assertEquals("Container ID is missing for item at index 1. All items must have a valid ID.", exception.getMessage());
  }

  // ---- validateContainerNumberUniqueness ----

  @Test
  void testValidateContainerNumberUniqueness_withUniqueNumber_shouldPass() {
    List<Containers> containers = List.of(
        Containers.builder().containerNumber("1234567891").build(),
        Containers.builder().containerNumber("2345678901").build()
    );

    assertDoesNotThrow(() -> containerValidationUtil.validateContainerNumberUniqueness("3456789012", containers));
  }

  @Test
  void testValidateContainerNumberUniqueness_withDuplicate_shouldThrowException() {
    List<Containers> containers = List.of(
        Containers.builder().containerNumber("1234567891").build(),
        Containers.builder().containerNumber("2345678901").build()
    );

    Exception exception = assertThrows(IllegalArgumentException.class,
        () -> containerValidationUtil.validateContainerNumberUniqueness("1234567891", containers));

    assertEquals("Container number '1234567891' already exists.", exception.getMessage());
  }

  @Test
  void testValidateContainerNumberUniqueness_withEmptyOrShortNumber_shouldDoNothing() {
    List<Containers> containers = List.of(
        Containers.builder().containerNumber("1234567891").build()
    );

    assertDoesNotThrow(() -> containerValidationUtil.validateContainerNumberUniqueness("", containers));
    assertDoesNotThrow(() -> containerValidationUtil.validateContainerNumberUniqueness(null, containers));
    assertDoesNotThrow(() -> containerValidationUtil.validateContainerNumberUniqueness("123", containers));
  }

}
