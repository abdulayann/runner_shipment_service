package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.EVgm;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEVgmRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EVgmDaoTest {
    @Mock
    private IEVgmRepository eVgmRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private EVgmDao eVgmDao;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidEVgm_ReturnsNewEVgm() {
        EVgm eVgm = new EVgm();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(eVgmRepository.save(any(EVgm.class))).thenReturn(eVgm);

        EVgm savedEVgm = eVgmDao.save(eVgm);

        assertEquals(eVgm, savedEVgm);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(eVgmRepository).save(any(EVgm.class));
    }

    @Test
    void save_ValidEVgm_ReturnsSavedEVgm() {
        EVgm eVgm = new EVgm();
        eVgm.setId(1L);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(eVgmRepository.save(any(EVgm.class))).thenReturn(eVgm);
        when(eVgmRepository.findById(1L)).thenReturn(Optional.of(eVgm));

        EVgm savedEVgm = eVgmDao.save(eVgm);

        assertEquals(eVgm, savedEVgm);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(eVgmRepository).save(any(EVgm.class));
    }

    @Test
    void save_ValidEVgm_ThrowsValidationException() {
        EVgm eVgm = new EVgm();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Set<String> set = Set.of("abcd", "defg");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(set);
        assertThrows(ValidationException.class, () -> eVgmDao.save(eVgm));
    }

    @Test
    void save_ExistingEVgm_ThrowsValidationException() {
        EVgm eVgm = new EVgm();
        eVgm.setId(1L);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(eVgmRepository.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> eVgmDao.save(eVgm));
    }


    @Test
    void saveAll_ValidEVgms_ReturnsSavedEVgms() {
        List<EVgm> eVgmList = Arrays.asList(new EVgm(), new EVgm());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(eVgmRepository.saveAll(anyList())).thenReturn(eVgmList);

        List<EVgm> savedEVgms = eVgmDao.saveAll(eVgmList);

        assertEquals(eVgmList, savedEVgms);
        verify(validatorUtility, times(2)).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(eVgmRepository).saveAll(eVgmList);
    }

    @Test
    void saveAll_ValidEVgms_ThrowsValidationException() {
        List<EVgm> eVgmList = Arrays.asList(new EVgm(), new EVgm());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Set.of("test"));
        assertThrows(ValidationException.class, () -> eVgmDao.saveAll(eVgmList));
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfEVgms() {
        Page<EVgm> expectedPage = mock(Page.class);
        Specification<EVgm> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(eVgmRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<EVgm> resultPage = eVgmDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(eVgmRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfEVgm() {
        Long id = 1L;
        EVgm eVgm = new EVgm();
        when(eVgmRepository.findById(id)).thenReturn(Optional.of(eVgm));

        Optional<EVgm> result = eVgmDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(eVgm, result.get());
        verify(eVgmRepository).findById(id);
    }

    @Test
    void findByGuid_ValidId_ReturnsOptionalOfEVgm() {
        EVgm eVgm = new EVgm();
        when(eVgmRepository.findByGuid(any())).thenReturn(Optional.of(eVgm));

        Optional<EVgm> result = eVgmDao.findByGuid(any());

        assertTrue(result.isPresent());
        assertEquals(eVgm, result.get());
        verify(eVgmRepository).findByGuid(any());
    }
}
