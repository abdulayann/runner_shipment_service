package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShippingInstructionRepository;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShippingInstructionDaoTest {

    @Mock
    private IShippingInstructionRepository shippingInstructionRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private ShippingInstructionDao shippingInstructionDao;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void save_ValidShippingInstruction_ReturnsNewShippingInstruction() {
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(shippingInstructionRepository.save(any(ShippingInstruction.class))).thenReturn(shippingInstruction);

        ShippingInstruction savedShippingInstruction = shippingInstructionDao.save(shippingInstruction);

        assertEquals(shippingInstruction, savedShippingInstruction);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(shippingInstructionRepository).save(any(ShippingInstruction.class));
    }

    @Test
    void save_ValidShippingInstruction_ReturnsSavedShippingInstruction() {
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(1L);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(shippingInstructionRepository.save(any(ShippingInstruction.class))).thenReturn(shippingInstruction);
        when(shippingInstructionRepository.findById(1L)).thenReturn(Optional.of(shippingInstruction));

        ShippingInstruction savedShippingInstruction = shippingInstructionDao.save(shippingInstruction);

        assertEquals(shippingInstruction, savedShippingInstruction);
        verify(validatorUtility).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(shippingInstructionRepository).save(any(ShippingInstruction.class));
    }

    @Test
    void save_ValidShippingInstruction_ThrowsValidationException() {
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        when(jsonHelper.convertToJson(any())).thenReturn("");
        Set<String> set = Set.of("abcd", "defg");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(set);
        assertThrows(ValidationException.class, () -> shippingInstructionDao.save(shippingInstruction));
    }

    @Test
    void save_ExistingShippingInstruction_ThrowsValidationException() {
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        shippingInstruction.setId(1L);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(shippingInstructionRepository.findById(1L)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> shippingInstructionDao.save(shippingInstruction));
    }


    @Test
    void saveAll_ValidShippingInstructions_ReturnsSavedShippingInstructions() {
        List<ShippingInstruction> shippingInstructionList = Arrays.asList(new ShippingInstruction(), new ShippingInstruction());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Collections.emptySet());
        when(shippingInstructionRepository.saveAll(anyList())).thenReturn(shippingInstructionList);

        List<ShippingInstruction> savedShippingInstructions = shippingInstructionDao.saveAll(shippingInstructionList);

        assertEquals(shippingInstructionList, savedShippingInstructions);
        verify(validatorUtility, times(2)).applyValidation(anyString(), anyString(), any(), anyBoolean());
        verify(shippingInstructionRepository).saveAll(shippingInstructionList);
    }

    @Test
    void saveAll_ValidShippingInstructions_ThrowsValidationException() {
        List<ShippingInstruction> shippingInstructionList = Arrays.asList(new ShippingInstruction(), new ShippingInstruction());
        when(jsonHelper.convertToJson(any())).thenReturn("");
        when(validatorUtility.applyValidation(anyString(), anyString(), any(), anyBoolean())).thenReturn(Set.of("test"));
        assertThrows(ValidationException.class, () -> shippingInstructionDao.saveAll(shippingInstructionList));
    }

    @Test
    void findAll_ValidSpecificationAndPageable_ReturnsPageOfShippingInstructions() {
        Page<ShippingInstruction> expectedPage = mock(Page.class);
        Specification<ShippingInstruction> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(shippingInstructionRepository.findAll(spec, pageable)).thenReturn(expectedPage);

        Page<ShippingInstruction> resultPage = shippingInstructionDao.findAll(spec, pageable);

        assertEquals(expectedPage, resultPage);
        verify(shippingInstructionRepository).findAll(spec, pageable);
    }

    @Test
    void findById_ValidId_ReturnsOptionalOfShippingInstruction() {
        Long id = 1L;
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        when(shippingInstructionRepository.findById(id)).thenReturn(Optional.of(shippingInstruction));

        Optional<ShippingInstruction> result = shippingInstructionDao.findById(id);

        assertTrue(result.isPresent());
        assertEquals(shippingInstruction, result.get());
        verify(shippingInstructionRepository).findById(id);
    }

    @Test
    void findByGuid_ValidId_ReturnsOptionalOfShippingInstruction() {
        ShippingInstruction shippingInstruction = new ShippingInstruction();
        when(shippingInstructionRepository.findByGuid(any())).thenReturn(Optional.of(shippingInstruction));

        Optional<ShippingInstruction> result = shippingInstructionDao.findByGuid(any());

        assertTrue(result.isPresent());
        assertEquals(shippingInstruction, result.get());
        verify(shippingInstructionRepository).findByGuid(any());
    }
}
