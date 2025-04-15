package com.dpw.runner.shipment.services.dao.impl;


import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentOrderRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.EntityManager;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ShipmentOrderDaoTest extends CommonMocks {

    @InjectMocks
    private ShipmentOrderDao shipmentOrderDao;

    @Mock
    private EntityManager entityManager;

    @Mock
    private IShipmentOrderRepository shipmentOrderRepository;
    @Mock
    private ValidatorUtility validatorUtility;
    @Mock
    private JsonHelper jsonHelper;


    @Test
    void saveTest() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        when(shipmentOrderRepository.save(any(ShipmentOrder.class))).thenReturn(shipmentOrder);
        assertEquals(shipmentOrder, shipmentOrderDao.save(shipmentOrder));
    }


    @Test
    void findAllTest() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        List<ShipmentOrder> shipmentOrderList = new ArrayList<>();
        shipmentOrderList.add(shipmentOrder);

        PageImpl<ShipmentOrder> shipmentOrdersPage = new PageImpl<>(shipmentOrderList);
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<ShipmentOrder>, Pageable> pair = fetchData(listReq, ShipmentOrder.class);

        when(shipmentOrderRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(shipmentOrdersPage);
        assertEquals(shipmentOrdersPage, shipmentOrderDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findByIdTest() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        var optionalShipment = Optional.of(shipmentOrder);
        when(shipmentOrderRepository.findById(any())).thenReturn(optionalShipment);
        assertEquals(optionalShipment, shipmentOrderDao.findById(1L));
    }

    @Test
    void findByShipmentIdAndOrderGuidTest() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        shipmentOrder.setShipmentId(11L);
        UUID uuid = UUID.fromString("26929514-237c-11ed-861d-0242ac120002");
        shipmentOrder.setOrderGuid(uuid);
        var optionalShipment = Optional.of(shipmentOrder);
        when(shipmentOrderRepository.findByShipmentIdAndOrderGuid(any(), any())).thenReturn(optionalShipment);
        Optional<ShipmentOrder> response =  shipmentOrderDao.findByShipmentIdAndOrderGuid(11L, uuid);
        assertEquals(optionalShipment, response);
        assertEquals(uuid, response.get().getOrderGuid());
    }

    @Test
    void findByShipmentIdTest() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        shipmentOrder.setShipmentId(11L);
        when(shipmentOrderRepository.findByShipmentId(any())).thenReturn(Collections.singletonList(shipmentOrder));
        List<ShipmentOrder> response =  shipmentOrderDao.findByShipmentId(11L);
        assertEquals(shipmentOrder, response.get(0));
        assertEquals(1L, response.get(0).getId());
    }

    @Test
    void updateEntityFromShipmentTest() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setId(1L);
        shipmentOrder.setShipmentId(11L);
        when(shipmentOrderRepository.saveAll(any())).thenReturn(Collections.singletonList(shipmentOrder));
        List<ShipmentOrder> response =  shipmentOrderDao.updateEntityFromShipment(Collections.singletonList(shipmentOrder), 11L);
        assertEquals(shipmentOrder, response.get(0));
        assertEquals(1L, response.get(0).getId());
    }

    @Test
    void updateEntityFromShipmentTest2() {
        ShipmentOrder shipmentOrder = ShipmentOrder.builder().build();
        shipmentOrder.setShipmentId(11L);
        when(shipmentOrderRepository.saveAll(any())).thenReturn(null);
        List<ShipmentOrder> response =  shipmentOrderDao.updateEntityFromShipment(Collections.singletonList(shipmentOrder), 11L);
        assertNull(response);
    }

}
