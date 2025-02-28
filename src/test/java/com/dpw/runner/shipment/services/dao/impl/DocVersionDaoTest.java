package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.DocVersion;
import com.dpw.runner.shipment.services.entity.enums.DocVersionTypes;
import com.dpw.runner.shipment.services.repository.interfaces.IDocVersionRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class DocVersionDaoTest {

    @InjectMocks
    private DocVersionDao docVersionDao;

    @Mock
    private IDocVersionRepository docVersionRepository;

    @Test
    void save() {
        DocVersion docVersion = new DocVersion();
        Mockito.when(docVersionRepository.save(any())).thenReturn(docVersion);
        DocVersion docVersion_ = docVersionDao.save(any());
        assert(docVersion_ == docVersion);
    }

    @Test
    void findByEntityIdAndType() {
        DocVersion docVersion = new DocVersion();
        Mockito.when(docVersionRepository.findByEntityIdAndType(any(), any())).thenReturn(List.of(docVersion));
        List<DocVersion> docVersion_ = docVersionDao.findByEntityIdAndType(1L, DocVersionTypes.PRE_ALERT);
        assert(docVersion_.size() == 1);
    }

}
