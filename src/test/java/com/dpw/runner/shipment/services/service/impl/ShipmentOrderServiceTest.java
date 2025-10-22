package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentOrderDao;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;


@ContextConfiguration(classes = {ShipmentOrderService.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentOrderServiceTest {
    @Mock
    IShipmentOrderDao shipmentOrderDao;
    @InjectMocks
    ShipmentOrderService shipmentOrderService;

    private MockMvc mockMvc;
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(shipmentOrderService).build();
    }

    @Test
    void testSave() {
        ShipmentOrder fakeShipmentOrder = ShipmentOrder.builder()
                .shipmentId(1L)
                .orderNumber("ORD-NUM")
                .orderGuid(UUID.randomUUID())
                .build();

        when(shipmentOrderDao.save(any())).thenReturn(fakeShipmentOrder);

        ShipmentOrder savedData = shipmentOrderService.save(fakeShipmentOrder);

        verify(shipmentOrderDao, times(1)).save(any());
        assertNotNull(savedData);
    }

    @Test
    void testFindAll() {
        Specification<ShipmentOrder> spec = (root, query, cb) -> cb.conjunction();
        PageRequest pageable = PageRequest.of(0, 10);
        ShipmentOrder order = ShipmentOrder.builder()
                .shipmentId(1L)
                .orderNumber("ORD-NUM")
                .orderGuid(UUID.randomUUID())
                .build();
        Page<ShipmentOrder> fakePage = new PageImpl<>(List.of(order));
        when(shipmentOrderDao.findAll(spec, pageable)).thenReturn(fakePage);

        Page<ShipmentOrder> result = shipmentOrderService.findAll(spec, pageable);

        assertEquals(1, result.getTotalElements());
        assertEquals("ORD-NUM", result.getContent().get(0).getOrderNumber());
        verify(shipmentOrderDao, times(1)).findAll(spec, pageable);
    }

    @Test
    void testFindById() {
        Optional<ShipmentOrder> fakeShipmentOrderOpt = Optional.ofNullable(ShipmentOrder.builder()
                .shipmentId(1L)
                .orderNumber("ORD-NUM")
                .orderGuid(UUID.randomUUID())
                .build());

        when(shipmentOrderDao.findById(any())).thenReturn(fakeShipmentOrderOpt);

        Optional<ShipmentOrder> savedData = shipmentOrderService.findById(1L);

        verify(shipmentOrderDao, times(1)).findById(any());
        assertNotNull(savedData);
        assertEquals(fakeShipmentOrderOpt, savedData);
    }

    @Test
    void testDelete() {
        ShipmentOrder order = ShipmentOrder.builder()
                .shipmentId(1L)
                .orderNumber("ORD-123")
                .build();

        shipmentOrderService.delete(order);

        verify(shipmentOrderDao, times(1)).delete(order);
    }

    @Test
    void testFindByShipmentId() {
        Long shipmentId = 10L;
        ShipmentOrder order = ShipmentOrder.builder()
                .shipmentId(shipmentId)
                .orderNumber("ORD-456")
                .build();

        when(shipmentOrderDao.findByShipmentId(shipmentId)).thenReturn(List.of(order));

        List<ShipmentOrder> result = shipmentOrderService.findByShipmentId(shipmentId);

        assertEquals(1, result.size());
        assertEquals("ORD-456", result.get(0).getOrderNumber());

        verify(shipmentOrderDao, times(1)).findByShipmentId(shipmentId);
    }

    @Test
    void testFindByShipmentIdAndOrderGuid() {
        Long shipmentId = 20L;
        UUID guid = UUID.randomUUID();
        ShipmentOrder order = ShipmentOrder.builder()
                .shipmentId(shipmentId)
                .orderGuid(guid)
                .orderNumber("ORD-789")
                .build();

        when(shipmentOrderDao.findByShipmentIdAndOrderGuid(shipmentId, guid)).thenReturn(Optional.of(order));

        Optional<ShipmentOrder> result = shipmentOrderService.findByShipmentIdAndOrderGuid(shipmentId, guid);

        assertTrue(result.isPresent());
        assertEquals("ORD-789", result.get().getOrderNumber());

        verify(shipmentOrderDao, times(1)).findByShipmentIdAndOrderGuid(shipmentId, guid);
    }

    @Test
    void testUpdateEntityFromShipment() {
        Long shipmentId = 30L;
        ShipmentOrder order = ShipmentOrder.builder()
                .shipmentId(shipmentId)
                .orderNumber("ORD-111")
                .build();

        List<ShipmentOrder> orders = List.of(order);

        when(shipmentOrderDao.updateEntityFromShipment(orders, shipmentId)).thenReturn(orders);

        List<ShipmentOrder> result = shipmentOrderService.updateEntityFromShipment(orders, shipmentId);

        assertEquals(1, result.size());
        assertEquals("ORD-111", result.get(0).getOrderNumber());

        verify(shipmentOrderDao, times(1)).updateEntityFromShipment(orders, shipmentId);
    }

    @Test
    void testDeleteAdditionalShipmentOrderByShipmentId() {
        List<Long> ids = List.of(1L, 2L, 3L);
        Long shipmentId = 40L;

        shipmentOrderService.deleteAdditionalShipmentOrderByShipmentId(ids, shipmentId);

        verify(shipmentOrderDao, times(1)).deleteAdditionalShipmentOrderByShipmentId(ids, shipmentId);
    }

    @Test
    void testRevertSoftDeleteByShipmentOrderIdsAndShipmentId() {
        List<Long> ids = List.of(5L, 6L);
        Long shipmentId = 50L;

        shipmentOrderService.revertSoftDeleteByshipmentOrderIdsAndShipmentId(ids, shipmentId);

        verify(shipmentOrderDao, times(1)).revertSoftDeleteByshipmentOrderIdsAndShipmentId(ids, shipmentId);
    }
}