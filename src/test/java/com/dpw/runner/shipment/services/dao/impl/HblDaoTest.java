package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.repository.interfaces.IHblRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@Execution(CONCURRENT)
@ExtendWith(MockitoExtension.class)
class HblDaoTest {

    @Mock
    private IHblRepository hblRepository;

    @InjectMocks
    private HblDao hblDao;

    private Hbl hbl;

    @BeforeEach
    void setUp() {
        hbl = new Hbl();
        hbl.setId(1L);
        hbl.setShipmentId(100L);
    }

    @Test
    void testSave() {
        when(hblRepository.save(hbl)).thenReturn(hbl);
        Hbl savedHbl = hblDao.save(hbl);
        assertEquals(hbl, savedHbl);
        verify(hblRepository, times(1)).save(hbl);
    }

    @Test
    void testFindByShipmentId() {
        List<Hbl> hblList = Arrays.asList(hbl);
        when(hblRepository.findByShipmentId(100L)).thenReturn(hblList);
        List<Hbl> foundHbls = hblDao.findByShipmentId(100L);
        assertEquals(hblList, foundHbls);
        verify(hblRepository, times(1)).findByShipmentId(100L);
    }

    @Test
    void testFindById() {
        when(hblRepository.findById(1L)).thenReturn(Optional.of(hbl));
        Optional<Hbl> foundHbl = hblDao.findById(1L);
        assertEquals(Optional.of(hbl), foundHbl);
        verify(hblRepository, times(1)).findById(1L);
    }

    @Test
    void testDelete() {
        doNothing().when(hblRepository).delete(hbl);
        hblDao.delete(hbl);
        verify(hblRepository, times(1)).delete(hbl);
    }
}
