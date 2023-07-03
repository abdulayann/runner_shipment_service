package com.dpw.runner.shipment.services.utils;

import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.DefaultTransactionStatus;

import javax.sql.DataSource;

public class CustomTransactionManager extends DataSourceTransactionManager {

    public CustomTransactionManager(DataSource dataSource) {
        super(dataSource);
    }

    @Override
    protected void doCommit(DefaultTransactionStatus status) throws TransactionException {
        super.doCommit(status);
    }

    @Override
    protected void doRollback(DefaultTransactionStatus status) throws TransactionException {
        super.doRollback(status);
    }

}
