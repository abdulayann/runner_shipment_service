package com.dpw.runner.shipment.services.dao.impl;


import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.repository.interfaces.IDefaultViewsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DefaultViewsDaoTest {

    @Mock
    private IDefaultViewsRepository defaultViewsRepository;

    @InjectMocks
    private DefaultViewsDao defaultViewsDao;

    private DefaultViews defaultView;

    @BeforeEach
    void setUp() {
        defaultView = new DefaultViews();
        defaultView.setId(1L);
        defaultView.setDefaultViewId(100L);
        defaultView.setUsername("testUser");
    }

    @Test
    void testSave() {
        when(defaultViewsRepository.save(defaultView)).thenReturn(defaultView);
        DefaultViews savedDefaultView = defaultViewsDao.save(defaultView);
        assertEquals(defaultView, savedDefaultView);
        verify(defaultViewsRepository, times(1)).save(defaultView);
    }

    @Test
    void testFindAll() {
        List<DefaultViews> defaultViewList = Arrays.asList(defaultView);
        when(defaultViewsRepository.findAll()).thenReturn(defaultViewList);
        List<DefaultViews> foundDefaultViews = defaultViewsDao.findAll();
        assertEquals(defaultViewList, foundDefaultViews);
        verify(defaultViewsRepository, times(1)).findAll();
    }

    @Test
    void testFindById() {
        when(defaultViewsRepository.findById(1L)).thenReturn(Optional.of(defaultView));
        Optional<DefaultViews> foundDefaultView = defaultViewsDao.findById(1L);
        assertEquals(Optional.of(defaultView), foundDefaultView);
        verify(defaultViewsRepository, times(1)).findById(1L);
    }

    @Test
    void testDelete() {
        doNothing().when(defaultViewsRepository).delete(defaultView);
        defaultViewsDao.delete(defaultView);
        verify(defaultViewsRepository, times(1)).delete(defaultView);
    }

    @Test
    void testFindByDefaultViewId() {
        when(defaultViewsRepository.findByDefaultViewId(100L)).thenReturn(Optional.of(defaultView));
        Optional<DefaultViews> foundDefaultView = defaultViewsDao.findByDefaultViewId(100L);
        assertEquals(Optional.of(defaultView), foundDefaultView);
        verify(defaultViewsRepository, times(1)).findByDefaultViewId(100L);
    }

    @Test
    void testFindByUsername() {
        when(defaultViewsRepository.findByUsername("testUser")).thenReturn(Optional.of(defaultView));
        Optional<DefaultViews> foundDefaultView = defaultViewsDao.findByUsername("testUser");
        assertEquals(Optional.of(defaultView), foundDefaultView);
        verify(defaultViewsRepository, times(1)).findByUsername("testUser");
    }
}