package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.SequenceIncrementor;
import com.dpw.runner.shipment.services.repository.interfaces.ISequenceIncrementorRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class SequenceIncrementorDaoTest {

    @InjectMocks
    private SequenceIncrementorDao sequenceIncrementorDao;

    @Mock
    private ISequenceIncrementorRepository sequenceIncrementorRepository;

    @Test
    void save() {
        SequenceIncrementor sequenceIncrementor = SequenceIncrementor.builder().build();
        when(sequenceIncrementorRepository.save(any())).thenReturn(sequenceIncrementor);
        assertEquals(sequenceIncrementor, sequenceIncrementorDao.save(sequenceIncrementor));
    }

    @Test
    void delete() {
        SequenceIncrementor sequenceIncrementor = SequenceIncrementor.builder().build();
        sequenceIncrementorDao.delete(sequenceIncrementor);
        verify(sequenceIncrementorRepository, times(1)).delete(sequenceIncrementor);
    }
}
