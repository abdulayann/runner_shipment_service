package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.repository.interfaces.IValidationsRepository;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ValidationsDaoTest {

    @InjectMocks
    private ValidationsDao validationsDao;

    @Mock
    private IValidationsRepository validationsRepository;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        Validations validations = Validations.builder().build();
        when(validationsRepository.save(any())).thenReturn(validations);
        assertEquals(validations, validationsDao.save(validations));
    }

    @Test
    void findAllPage() {
        ListCommonRequest listReq = constructListCommonRequest("id", 1, "=");
        Pair<Specification<Validations>, Pageable> pair = fetchData(listReq, Validations.class);
        assertEquals(null, validationsDao.findAll(pair.getLeft(), pair.getRight()));
    }

    @Test
    void findById() {
        Validations validations = Validations.builder().build();
        when(validationsRepository.findById(any())).thenReturn(Optional.of(validations));
        assertEquals(validations, validationsDao.findById(1L).get());
    }

    @Test
    void delete() {
        Validations validations = Validations.builder().build();
        validationsDao.delete(validations);
        verify(validationsRepository, times(1)).delete(validations);
    }

    @Test
    void findAllList() {
        Validations validations = Validations.builder().build();
        List<Validations> validationsList = Arrays.asList(validations);
        when(validationsRepository.findAll()).thenReturn(validationsList);
        assertEquals(validationsList, validationsDao.findAll());
    }

    @Test
    void findByLifecycleHookAndEntity() {
        Validations validations = Validations.builder().build();
        List<Validations> validationsList = Arrays.asList(validations);
        LifecycleHooks lifecycleHooks = LifecycleHooks.ON_SAVE;
        when(validationsRepository.findByLifecycleHookAndEntity(any(), any(), any())).thenReturn(Optional.of(validationsList));
        assertEquals(validationsList, validationsDao.findByLifecycleHookAndEntity(LifecycleHooks.ON_SAVE, "entity").get());
    }
}
