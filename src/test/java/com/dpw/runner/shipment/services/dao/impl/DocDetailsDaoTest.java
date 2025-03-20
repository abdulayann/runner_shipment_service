package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.DocDetails;
import com.dpw.runner.shipment.services.entity.enums.DocDetailsTypes;
import com.dpw.runner.shipment.services.repository.interfaces.IDocDetailsRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DocDetailsDaoTest {

    @InjectMocks
    private DocDetailsDao docDetailsDao;

    @Mock
    private IDocDetailsRepository docDetailsRepository;

    @Test
    void save() {
        DocDetails docDetails = new DocDetails();
        Mockito.when(docDetailsRepository.save(any())).thenReturn(docDetails);
        DocDetails docDetails1 = docDetailsDao.save(any());
        assertEquals(docDetails1, docDetails);
    }

    @Test
    void findByEntityIdAndType() {
        DocDetails docDetails = new DocDetails();
        Mockito.when(docDetailsRepository.findByEntityIdAndType(any(), any())).thenReturn(List.of(docDetails));
        List<DocDetails> docDetails1 = docDetailsDao.findByEntityIdAndType(1L, DocDetailsTypes.PRE_ALERT);
        assertEquals(1, docDetails1.size());
    }

}
