package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleIdProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleNoProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShippingInstructionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShippingInstructionDaoTest {

    @Mock
    private IShippingInstructionRepository shippingInstructionRepository;

    @InjectMocks
    private ShippingInstructionDao shippingInstructionDao;

    private ShippingInstruction testShippingInstruction;
    private Long testId;

    @BeforeEach
    void setUp() {
        testId = 1L;
        testShippingInstruction = ShippingInstruction.builder()
                .carrierBlNo("BL123456")
                .carrierBookingNo("BK123456")
                .entityType(EntityType.CONSOLIDATION)
                .entityId(100L)
                .entityNumber("CONSOL-001")
                .build();
    }

    @Test
    void testCreate_Success() {
        // Arrange
        when(shippingInstructionRepository.save(any(ShippingInstruction.class)))
                .thenReturn(testShippingInstruction);

        // Act
        ShippingInstruction result = shippingInstructionDao.create(testShippingInstruction);

        // Assert
        assertNotNull(result);
        assertEquals(testShippingInstruction.getCarrierBlNo(), result.getCarrierBlNo());
        assertEquals(testShippingInstruction.getCarrierBookingNo(), result.getCarrierBookingNo());
        verify(shippingInstructionRepository, times(1)).save(testShippingInstruction);
    }

    @Test
    void testCreate_WithNullEntity() {
        // Arrange
        when(shippingInstructionRepository.save(null)).thenReturn(null);

        // Act
        ShippingInstruction result = shippingInstructionDao.create(null);

        // Assert
        assertNull(result);
        verify(shippingInstructionRepository, times(1)).save(null);
    }

    @Test
    void testFindById_Success() {
        // Arrange
        when(shippingInstructionRepository.findById(testId))
                .thenReturn(Optional.of(testShippingInstruction));

        // Act
        Optional<ShippingInstruction> result = shippingInstructionDao.findById(testId);

        // Assert
        assertTrue(result.isPresent());
        assertEquals(testShippingInstruction.getCarrierBlNo(), result.get().getCarrierBlNo());
        verify(shippingInstructionRepository, times(1)).findById(testId);
    }

    @Test
    void testFindById_NotFound() {
        // Arrange
        when(shippingInstructionRepository.findById(testId))
                .thenReturn(Optional.empty());

        // Act
        Optional<ShippingInstruction> result = shippingInstructionDao.findById(testId);

        // Assert
        assertFalse(result.isPresent());
        verify(shippingInstructionRepository, times(1)).findById(testId);
    }

    @Test
    void testFindAll_Success() {
        // Arrange
        Specification<ShippingInstruction> spec = mock(Specification.class);
        Pageable pageable = PageRequest.of(0, 10);
        List<ShippingInstruction> instructions = Arrays.asList(testShippingInstruction);
        Page<ShippingInstruction> expectedPage = new PageImpl<>(instructions, pageable, 1);

        when(shippingInstructionRepository.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(expectedPage);

        // Act
        Page<ShippingInstruction> result = shippingInstructionDao.findAll(spec, pageable);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.getTotalElements());
        assertEquals(1, result.getContent().size());
        assertEquals(testShippingInstruction.getCarrierBlNo(), result.getContent().get(0).getCarrierBlNo());
        verify(shippingInstructionRepository, times(1)).findAll(spec, pageable);
    }

    @Test
    void testFindAll_EmptyResult() {
        // Arrange
        Specification<ShippingInstruction> spec = mock(Specification.class);
        Pageable pageable = PageRequest.of(0, 10);
        Page<ShippingInstruction> emptyPage = new PageImpl<>(Arrays.asList(), pageable, 0);

        when(shippingInstructionRepository.findAll(any(Specification.class), any(Pageable.class)))
                .thenReturn(emptyPage);

        // Act
        Page<ShippingInstruction> result = shippingInstructionDao.findAll(spec, pageable);

        // Assert
        assertNotNull(result);
        assertEquals(0, result.getTotalElements());
        assertTrue(result.getContent().isEmpty());
        verify(shippingInstructionRepository, times(1)).findAll(spec, pageable);
    }

    @Test
    void testUpdate_Success() {
        // Arrange
        when(shippingInstructionRepository.save(any(ShippingInstruction.class)))
                .thenReturn(testShippingInstruction);

        // Act
        ShippingInstruction result = shippingInstructionDao.update(testId, testShippingInstruction);

        // Assert
        assertNotNull(result);
        assertEquals(testShippingInstruction.getCarrierBlNo(), result.getCarrierBlNo());
        verify(shippingInstructionRepository, times(1)).save(testShippingInstruction);
    }

    @Test
    void testDelete_Success() {
        // Arrange
        doNothing().when(shippingInstructionRepository).delete(any(ShippingInstruction.class));

        // Act
        shippingInstructionDao.delete(testShippingInstruction);

        // Assert
        verify(shippingInstructionRepository, times(1)).delete(testShippingInstruction);
    }

    @Test
    void testSave_Success() {
        // Arrange
        when(shippingInstructionRepository.save(any(ShippingInstruction.class)))
                .thenReturn(testShippingInstruction);

        // Act
        ShippingInstruction result = shippingInstructionDao.save(testShippingInstruction);

        // Assert
        assertNotNull(result);
        assertEquals(testShippingInstruction.getCarrierBlNo(), result.getCarrierBlNo());
        verify(shippingInstructionRepository, times(1)).save(testShippingInstruction);
    }

    @Test
    void testFindBookingInfoById_Success() {
        // Arrange
        CarrierBookingInfoProjection mockProjection = mock(CarrierBookingInfoProjection.class);
        when(mockProjection.getBookingStatus()).thenReturn("CONFIRMED");
        when(mockProjection.getBookingNo()).thenReturn("BK123456");
        when(shippingInstructionRepository.findCarrierBookingInfoById(testId))
                .thenReturn(mockProjection);

        // Act
        CarrierBookingInfoProjection result = shippingInstructionDao.findBookingInfoById(testId);

        // Assert
        assertNotNull(result);
        assertEquals("CONFIRMED", result.getBookingStatus());
        assertEquals("BK123456", result.getBookingNo());
        verify(shippingInstructionRepository, times(1)).findCarrierBookingInfoById(testId);
    }

    @Test
    void testFindBookingInfoById_NotFound() {
        // Arrange
        when(shippingInstructionRepository.findCarrierBookingInfoById(testId))
                .thenReturn(null);

        // Act
        CarrierBookingInfoProjection result = shippingInstructionDao.findBookingInfoById(testId);

        // Assert
        assertNull(result);
        verify(shippingInstructionRepository, times(1)).findCarrierBookingInfoById(testId);
    }

    @Test
    void testExistsById_True() {
        // Arrange
        when(shippingInstructionRepository.existsById(testId)).thenReturn(true);

        // Act
        boolean result = shippingInstructionDao.existsById(testId);

        // Assert
        assertTrue(result);
        verify(shippingInstructionRepository, times(1)).existsById(testId);
    }

    @Test
    void testExistsById_False() {
        // Arrange
        when(shippingInstructionRepository.existsById(testId)).thenReturn(false);

        // Act
        boolean result = shippingInstructionDao.existsById(testId);

        // Assert
        assertFalse(result);
        verify(shippingInstructionRepository, times(1)).existsById(testId);
    }

    @Test
    void testFindByEntityTypeAndEntityNoIn_Success() {
        // Arrange
        EntityType entityType = EntityType.CONSOLIDATION;
        List<String> consolNumbers = Arrays.asList("CONSOL-001", "CONSOL-002");
        ShippingConsoleNoProjection mockProjection = mock(ShippingConsoleNoProjection.class);
        when(mockProjection.getId()).thenReturn(1L);
        when(mockProjection.getEntityId()).thenReturn("CONSOL-001");
        List<ShippingConsoleNoProjection> expectedList = Arrays.asList(mockProjection);

        when(shippingInstructionRepository.findByEntityTypeAndEntityNo(entityType.name(), consolNumbers))
                .thenReturn(expectedList);

        // Act
        List<ShippingConsoleNoProjection> result = shippingInstructionDao
                .findByEntityTypeAndEntityNoIn(entityType, consolNumbers);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(1L, result.get(0).getId());
        assertEquals("CONSOL-001", result.get(0).getEntityId());
        verify(shippingInstructionRepository, times(1))
                .findByEntityTypeAndEntityNo(entityType.name(), consolNumbers);
    }

    @Test
    void testFindByEntityTypeAndEntityNoIn_EmptyList() {
        // Arrange
        EntityType entityType = EntityType.CONSOLIDATION;
        List<String> consolNumbers = Arrays.asList();
        List<ShippingConsoleNoProjection> expectedList = Arrays.asList();

        when(shippingInstructionRepository.findByEntityTypeAndEntityNo(entityType.name(), consolNumbers))
                .thenReturn(expectedList);

        // Act
        List<ShippingConsoleNoProjection> result = shippingInstructionDao
                .findByEntityTypeAndEntityNoIn(entityType, consolNumbers);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(shippingInstructionRepository, times(1))
                .findByEntityTypeAndEntityNo(entityType.name(), consolNumbers);
    }


    @Test
    void testFindByEntityTypeAndEntityIdIn_Success() {
        // Arrange
        EntityType entityType = EntityType.CONSOLIDATION;
        List<Long> consolIds = Arrays.asList(100L, 101L);
        ShippingConsoleIdProjection mockProjection = mock(ShippingConsoleIdProjection.class);
        when(mockProjection.getId()).thenReturn(1L);
        when(mockProjection.getEntityId()).thenReturn(100L);
        List<ShippingConsoleIdProjection> expectedList = Arrays.asList(mockProjection);

        when(shippingInstructionRepository.findByEntityTypeAndEntityId(entityType.name(), consolIds))
                .thenReturn(expectedList);

        // Act
        List<ShippingConsoleIdProjection> result = shippingInstructionDao
                .findByEntityTypeAndEntityIdIn(entityType, consolIds);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(1L, result.get(0).getId());
        assertEquals(100L, result.get(0).getEntityId());
        verify(shippingInstructionRepository, times(1))
                .findByEntityTypeAndEntityId(entityType.name(), consolIds);
    }

    @Test
    void testFindByEntityTypeAndEntityIdIn_EmptyResult() {
        // Arrange
        EntityType entityType = EntityType.CARRIER_BOOKING;
        List<Long> consolIds = Arrays.asList(100L);
        List<ShippingConsoleIdProjection> expectedList = Arrays.asList();

        when(shippingInstructionRepository.findByEntityTypeAndEntityId(entityType.name(), consolIds))
                .thenReturn(expectedList);

        // Act
        List<ShippingConsoleIdProjection> result = shippingInstructionDao
                .findByEntityTypeAndEntityIdIn(entityType, consolIds);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(shippingInstructionRepository, times(1))
                .findByEntityTypeAndEntityId(entityType.name(), consolIds);
    }


    @Test
    void testFindBookingByConsolId_Success() {
        // Arrange
        Long entityId = 100L;
        CarrierBookingInfoProjection mockProjection = mock(CarrierBookingInfoProjection.class);
        when(mockProjection.getBookingStatus()).thenReturn("CONFIRMED");
        when(mockProjection.getBookingNo()).thenReturn("BK123456");
        List<CarrierBookingInfoProjection> expectedList = Arrays.asList(mockProjection);

        when(shippingInstructionRepository.findByConsolidationNo(entityId))
                .thenReturn(expectedList);

        // Act
        List<CarrierBookingInfoProjection> result = shippingInstructionDao
                .findBookingByConsolId(entityId);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("CONFIRMED", result.get(0).getBookingStatus());
        assertEquals("BK123456", result.get(0).getBookingNo());
        verify(shippingInstructionRepository, times(1)).findByConsolidationNo(entityId);
    }

    @Test
    void testFindBookingByConsolId_EmptyResult() {
        // Arrange
        Long entityId = 100L;
        List<CarrierBookingInfoProjection> expectedList = Arrays.asList();

        when(shippingInstructionRepository.findByConsolidationNo(entityId))
                .thenReturn(expectedList);

        // Act
        List<CarrierBookingInfoProjection> result = shippingInstructionDao
                .findBookingByConsolId(entityId);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(shippingInstructionRepository, times(1)).findByConsolidationNo(entityId);
    }

    @Test
    void testFindConfirmedBookingByConsolId_EmptyResult() {
        // Arrange
        String consolidationNumber = "CONSOL-999";
        List<CarrierBookingInfoProjection> expectedList = Arrays.asList();

        when(shippingInstructionRepository.findConfirmedBookingsByConsolidationNo(consolidationNumber))
                .thenReturn(expectedList);

        // Act
        List<CarrierBookingInfoProjection> result = shippingInstructionDao
                .findConfimedBookingByConsolId(consolidationNumber);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(shippingInstructionRepository, times(1))
                .findConfirmedBookingsByConsolidationNo(consolidationNumber);
    }

    @Test
    void testFindByEntityTypeAndEntityNoIn_WithCarrierBookingType() {
        // Arrange
        EntityType entityType = EntityType.CARRIER_BOOKING;
        List<String> consolNumbers = Arrays.asList("BK001", "BK002");
        ShippingConsoleNoProjection mockProjection = mock(ShippingConsoleNoProjection.class);
        when(mockProjection.getId()).thenReturn(2L);
        when(mockProjection.getEntityId()).thenReturn("BK001");
        List<ShippingConsoleNoProjection> expectedList = Arrays.asList(mockProjection);

        when(shippingInstructionRepository.findByEntityTypeAndEntityNo(entityType.name(), consolNumbers))
                .thenReturn(expectedList);

        // Act
        List<ShippingConsoleNoProjection> result = shippingInstructionDao
                .findByEntityTypeAndEntityNoIn(entityType, consolNumbers);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(2L, result.get(0).getId());
        assertEquals("BK001", result.get(0).getEntityId());
        verify(shippingInstructionRepository, times(1))
                .findByEntityTypeAndEntityNo(entityType.name(), consolNumbers);
    }

    @Test
    void testFindByEntityTypeAndEntityIdIn_WithCarrierBookingType() {
        // Arrange
        EntityType entityType = EntityType.CARRIER_BOOKING;
        List<Long> consolIds = Arrays.asList(200L, 201L);
        ShippingConsoleIdProjection mockProjection = mock(ShippingConsoleIdProjection.class);
        when(mockProjection.getId()).thenReturn(2L);
        when(mockProjection.getEntityId()).thenReturn(200L);
        List<ShippingConsoleIdProjection> expectedList = Arrays.asList(mockProjection);

        when(shippingInstructionRepository.findByEntityTypeAndEntityId(entityType.name(), consolIds))
                .thenReturn(expectedList);

        // Act
        List<ShippingConsoleIdProjection> result = shippingInstructionDao
                .findByEntityTypeAndEntityIdIn(entityType, consolIds);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(2L, result.get(0).getId());
        assertEquals(200L, result.get(0).getEntityId());
        verify(shippingInstructionRepository, times(1))
                .findByEntityTypeAndEntityId(entityType.name(), consolIds);
    }
}
