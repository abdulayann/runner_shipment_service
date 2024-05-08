//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.DefaultViews;
//import com.dpw.runner.shipment.services.entity.Views;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import com.dpw.runner.shipment.services.repository.interfaces.IViewsRepository;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.Mockito;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.data.domain.Page;
//import org.springframework.data.domain.PageImpl;
//import org.springframework.data.domain.Pageable;
//import org.springframework.data.jpa.domain.Specification;
//
//import java.io.IOException;
//import java.util.*;
//
//@ExtendWith(MockitoExtension.class)
//class ViewsDaoTest {
//
//    @Mock
//    private IViewsRepository viewsRepository;
//
//    private static JsonTestUtility jsonTestUtility;
//    private static DefaultViews testData;
//
//    @InjectMocks
//    private ViewsDao viewsDao;
//
//    @BeforeAll
//    static void beforeAll() throws IOException {
//        jsonTestUtility = new JsonTestUtility();
//    }
//
//    @BeforeEach
//    void setUp() {
//        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
//    }
//
//    @Test
//    void save() {
//        Views views = new Views();
//        Mockito.when(viewsRepository.save(Mockito.any())).thenReturn(views);
//        Views views1 = viewsDao.save(Mockito.any());
//        assert(views == views1);
//    }
//
//    @Test
//    void findAll() {
//        List<Views> viewsList = new ArrayList<>();
//        Mockito.when(viewsRepository.findAll()).thenReturn(viewsList);
//        List<Views> views = viewsDao.findAll();
//        assert(viewsList.size() == views.size());
//    }
//
//    @Test
//    void findAllWithSpec() {
//        Specification<Views> spec = null;
//        Pageable pageable = null;
//        List<Views> viewList = new ArrayList<>();
//        Page<Views> viewsList = new PageImpl<>(viewList);
//        Mockito.when(viewsRepository.findAll(spec, pageable)).thenReturn(viewsList);
//        Page<Views> views = viewsDao.findAll(spec, pageable);
//        assert(viewsList.getTotalElements() == views.getTotalElements());
//    }
//
//    @Test
//    void findById() {
//        Views views = new Views();
//        views.setId(1L);
//        Long id = 1L;
//        Mockito.when(viewsRepository.findById(Mockito.any())).thenReturn(Optional.of(views));
//        Optional<Views> views1 = viewsDao.findById(id);
//        assert(Objects.equals(views.getId(), views1.get().getId()));
//    }
//
//    @Test
//    void delete() {
//        Views views = new Views();
//        viewsDao.delete(views);
//    }
//}