package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.repository.interfaces.IViewsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ViewsDaoTest {

    @Mock
    private IViewsRepository viewsRepository;

    @InjectMocks
    private ViewsDao viewsDao;

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
    }

    @Test
    void save() {
        Views views = new Views();
        when(viewsRepository.save(Mockito.any())).thenReturn(views);
        Views views1 = viewsDao.save(Mockito.any());
        assertEquals(views, views1);
    }

    @Test
    void findAll() {
        List<Views> viewsList = new ArrayList<>();
        when(viewsRepository.findAll()).thenReturn(viewsList);
        List<Views> views = viewsDao.findAll();
        assertEquals(viewsList.size(), views.size());
    }

    @Test
    void findAllWithSpec() {
        Specification<Views> spec = null;
        Pageable pageable = PageRequest.of(0, 10);
        List<Views> viewList = new ArrayList<>();
        Page<Views> viewsList = new PageImpl<>(viewList);
        when(viewsRepository.findAll(spec, pageable)).thenReturn(viewsList);
        Page<Views> views = viewsDao.findAll(spec, pageable);
        assertEquals(viewsList.getTotalElements(), views.getTotalElements());
    }

    @Test
    void findById() {
        Views views = new Views();
        views.setId(1L);
        Long id = 1L;
        when(viewsRepository.findById(Mockito.any())).thenReturn(Optional.of(views));
        Optional<Views> views1 = viewsDao.findById(id);
        assert(views1.isPresent());
        assertEquals(views.getId(), views1.get().getId());
    }

    @Test
    void delete() {
        Views views = new Views();
        viewsDao.delete(views);
        verify(viewsRepository, times(1)).delete(views);
    }

    @Test
    void findAllByUsername() {
        when(viewsRepository.findAllByUsernameAndEntity(any(), any())).thenReturn(new ArrayList<>());
        List<String> response = viewsDao.findAllByUsernameAndEntity("egy", "entity");
        assertEquals(0, response.size());
    }

    @Test
    void findByCreatedByAndIsDefault() {
        when(viewsRepository.findByCreatedByAndIsDefault(any(), any())).thenReturn(new Views());
        Optional<Views> views = viewsDao.findByCreatedByAndEntityAndIsDefault("egy", "abc");
        assertTrue(views.isPresent());
    }

}